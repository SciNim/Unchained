import std / [tables, macros]
import core_types, quantities

type
  DefinedUnitType* = enum
    utBase, utDerived

  ## TODO: rename the `utBase` and `utDerived`. `utDerived` is confusing with the term
  ## "derived SI units" which are not what we mean here. We mean units that are non SI
  ## units that are defined by a conversion to an SI unit.
  DefinedUnit* = object
    id*: int ## unique ID of this defined unit. Allows to do comparisons / hashing based
             ## on ID only and generate a type similar to `QuantityPowerArray` for units (if needed)
    name*: string
    basePrefix*: SiPrefix ## This prefix defines the possible prefix of the *base unit*, which
                          ## in SI is only relevant for KiloGram
    short*: string
    quantity*: CTQuantity
    autoConvert*: bool
    case kind*: DefinedUnitType
    of utBase: discard
    of utDerived: conversion*: UnitProduct
    case quantityKind*: QuantityType
    of qtFundamental: toNaturalUnit*: UnitProduct
    else: discard

  UnitInstance* = object
    name*: string # the part of the nim node that was parsed into this
    unit*: DefinedUnit ## The actual underlying unit
    prefix*: SiPrefix # possibly differentiating prefix
    power*: int
    value*: float

  UnitProduct* = object # a product of multiple (possibly compound) units
    value*: float # value of this unit poduct
    units*: seq[UnitInstance]
    init*: bool
    #siPrefix*: float # as a pure float value
  UnitTable* = ref object
    # quantity stores the *base unit* referring to a quantity (the key)
    quantity*: Table[string, int]
    # long and short store indices to the `seq` `units`
    long*: Table[string, int]
    expandedCompoundName*: Table[string, int]
    longBaseUnits*: Table[string, int]
    short*: Table[string, int]
    units*: seq[DefinedUnit]
    userDefinedUnits*: Table[string, UnitProduct] ## maps any user defined unit to its
                                                  ## unit product. string is the name of the
                                                  ## unit as produced in `defUnit` for `resType`
                                                  ## The value is the user given type / generated type.
    unitNames*: Table[UnitProduct, string] ## Maps all units that have been converted to strings
                                           ## to their names.
    unitNamesLong*: Table[UnitProduct, string] ## Equivalent to `unitNames` for long names.

proc initUnitProduct*(value = 1.0): UnitProduct =
  result = UnitProduct(value: value, units: newSeq[UnitInstance](), init: true)

proc initDefinedUnit*(kind: DefinedUnitType,
                      id: int, ## unique ID for each defined unit
                      name: string,
                      basePrefix: SiPrefix,
                      short: string,
                      quantity: CTQuantity,
                      autoConvert: bool,
                      quantityKind: QuantityType,
                      conversion = initUnitProduct(),
                      toNaturalUnit = initUnitProduct()): DefinedUnit =
  result = DefinedUnit(kind: kind,
                       id: id,
                       name: name,
                       basePrefix: basePrefix,
                       short: short,
                       quantity: quantity,
                       autoConvert: autoConvert,
                       quantityKind: quantityKind)
                       #conversion: initUnitProduct(),
                       #toNaturalUnit: initUnitProduct())
  case quantityKind
  of qtFundamental: result.toNaturalUnit = toNaturalUnit
  else: discard
  case kind
  of utBase: discard
  of utDerived: result.conversion = conversion

proc len*(u: UnitProduct): int = u.units.len

proc newUnitInstance*(name: string,
                      u: DefinedUnit,
                      power: int,
                      prefix: SiPrefix,
                      value: float = 1.0): UnitInstance =
  result = UnitInstance(name: name,
                        unit: u,
                        power: power,
                        prefix: prefix,
                        value: value)

proc newUnitLess*(): UnitInstance =
  result = UnitInstance(
    name: "UnitLess",
    unit: DefinedUnit(
      name: "UnitLess",
      basePrefix: siIdentity,
      short: "",
      quantity: CTQuantity(
        kind: qtFundamental,
        b: CTBaseQuantity(
          name: "Dimensionless"
        )
      ),
      autoConvert: false,
      kind: utBase,
      quantityKind: qtFundamental,
      toNaturalUnit: initUnitProduct()
    ),
    prefix: siIdentity,
    power: 0,
    value: 1.0
  )

proc `$`*(d: DefinedUnit): string =
  result = "(DefinedUnit: "
  result.add $d.basePrefix & d.name & ", quantity: " & $d.quantity & ")"

proc `$`*(u: UnitInstance): string =
  result = "(UnitInstance: "
  result.add $u.prefix & $u.unit & "^" & $u.power & ")"

proc `$`*(u: UnitProduct): string =
  result = "(UnitProduct: "
  for i, unit in u.units:
    if i < u.units.high:
      result.add $unit & " "
    else:
      result.add $unit
  result.add ")"

proc `<`*(a, b: UnitInstance): bool =
  ## Comparison based on:
  ## - positive powers before negative powers
  ## - compound vs. non compound (compound first, i.e. `N•m` instead of `m•N`)
  ## - underlying unit's ID
  ## - the unit's power if same unit (higher power first)
  # 1. check if one is positive power and other negative,
  # if so return early and ignore actual units (so that
  # `inch•s⁻¹` remains this order, desipte lower precedence of
  # `inch` compared to `s`
  if a.power > 0 and b.power < 0:
    return true
  elif b.power > 0 and a.power < 0:
    return false

  # 2. if one unit is a compound unit, give it precedence over the
  # other non compound. So that we write `N•m` instead of `m•N`
  if a.unit.quantity.kind == qtCompound and b.unit.quantity.kind == qtFundamental:
    return true
  elif b.unit.quantity.kind == qtCompound and a.unit.quantity.kind == qtFundamental:
    return false

  # 3. if not returned take unit precedence into account, the unit's ID
  let aIdx = a.unit.id
  let bIdx = b.unit.id
  if aIdx < bIdx:
    result = true
  elif aIdx > bIdx:
    result = false
  else:
    ## NOTE: the power seem "inverted". This is because we wish to have units with
    ## larger powers ``in front`` of units with smaller powers. E.g.
    ## `Meter•Meter⁻¹` instead of `Meter⁻¹•Meter`
    ## We cannot sort in descending order, because the actual units in the `UnitKind`
    ## enum needs to be respected.
    if a.power > b.power:
      result = true
    elif a.power < b.power:
      result = false
    else:
      result = a.prefix < b.prefix

proc `==`*(a, b: DefinedUnit): bool =
  result = a.id == b.id

proc `==`*(a, b: UnitInstance): bool =
  result = (a.unit == b.unit and a.prefix == b.prefix and a.power == b.power)

proc toQuantityPower*(u: UnitInstance): QuantityPowerArray =
  ## Turns the given unit instance into a `QuantityPowerArray` for
  ## dimensional analysis.
  # 1. first convert to a QuantityPower based on the internal
  #    quantity of the unit
  # 2. correct the powers based on the power of the unit instance, e.g. to:
  #    N² = (kg•m•s⁻²)² = kg²•m²•s⁻⁴
  #     ^--- power of unit instance
  #                ^--- powers part of the unit's quantity
  # handled implicitly by QuantityPowerArray and QP `*` overload
  result = initQuantityPowerArray()
  for q in u.unit.quantity.quantityPowers:
    result.add (q * u.power)

import std / hashes
proc hash*(d: DefinedUnit): Hash =
  result = !$ hash(d.id)

proc hash*(u: UnitInstance): Hash =
  # ignore name, as it varies
  result = result !& hash(u.unit)
  result = result !& hash(u.prefix)
  result = result !& hash(u.power)
  result = result !& hash(u.value)
  result = !$result

proc add*(comp: var UnitProduct, unit: UnitInstance) =
  doAssert comp.init
  comp.units.add unit

proc add*(comp: var UnitProduct, toAdd: UnitProduct) =
  ## adding a sequence of compound units equates to multiplying units
  doAssert comp.init
  for u in toAdd.units:
    comp.add u
  comp.value *= toAdd.value

proc clone*(up: UnitProduct): UnitProduct =
  result = initUnitProduct(value = up.value)
  for units in up.units:
    result.add units

proc toUnitInstance*(u: DefinedUnit, assignPrefix = false): UnitInstance =
  ## `assignPrefix` can be used to overwrite assinging the base prefix to the
  ## resulting `UnitInstance.` if we lookup a unit from the `UnitTable` and we
  ## have a direct match for a unit, we do *not* want to assign the base prefix,
  ## as we have the literal unit, not its base.
  if assignPrefix:
    result = newUnitInstance(u.name, u, power = 1, prefix = u.basePrefix)
  else:
    result = newUnitInstance(u.name, u, power = 1, prefix = siIdentity)

proc toUnitProduct*(u: UnitInstance): UnitProduct =
  result = initUnitProduct()
  result.add u

## Procedures for `UnitTable`
proc newUnitTable*(): UnitTable =
  result = UnitTable(quantity: initTable[string, int](),
                     long: initTable[string, int](),
                     longBaseUnits: initTable[string, int](),
                     short: initTable[string, int](),
                     units: newSeq[DefinedUnit]())

proc len*(tab: UnitTable): int = tab.units.len

proc contains*(tab: UnitTable, u: string): bool =
  ## Checks if the given `u` is in the `UnitTable`
  result = u in tab.short or u in tab.long or u in tab.expandedCompoundName

proc isUserDefined*(tab: UnitTable, u: string): bool =
  result = u in tab.userDefinedUnits

proc insert*(tab: var UnitTable, u: DefinedUnit, hasConversion: bool,
             compoundName = "") =
  ## Inserts the given `u` into the `UnitTable`. The correct sub field will be filled
  ## based on `isLong`
  let idx = tab.len # get index as the current length
  if u.kind == utBase:
    let qName = u.quantity.getName()
    if qName in tab.quantity:
      error("The unit " & $u.name & " defines a quantity " & $qName & " that was already " &
        "defined by another unit: " & $tab.units[tab.quantity[qName]].repr & ". If this is not a " &
        "base unit, make sure to define a `conversion` to the base unit of the same quantity.")
    tab.quantity[qName] = idx

  tab.long[u.name] = idx
  if not hasConversion:
    tab.longBaseUnits[u.name] = idx
  if compoundName.len > 0:
    doAssert u.quantity.kind == qtCompound
    if compoundName notin tab:
      tab.expandedCompoundName[compoundName] = idx
      when defined(debugUnits):
        echo "INFO: skipping insertion of compound ", compoundName, " for unit: ", u.name, " due to ",
           "existing unit ", tab.units[tab.expandedCompoundName[compoundName]], " already present. ",
           "Change the order of the unit definitions if you prefer a different priority."
  tab.short[u.short] = idx
  tab.units.add u

proc insert*(tab: var UnitTable, unit: string, asUnit: UnitProduct) =
  ## Insert the given user defined unit `unit`
  if unit != "UnitLess" and unit notin tab and unit notin tab.userDefinedUnits:
    tab.userDefinedUnits[unit] = asUnit

proc `[]`*(tab: UnitTable, s: string): DefinedUnit =
  if s in tab.long:
    result = tab.units[tab.long[s]]
  elif s in tab.short:
    result = tab.units[tab.short[s]]
  elif s in tab.expandedCompoundName:
    result = tab.units[tab.expandedCompoundName[s]]
  else:
    raise newException(KeyError, "Given unit " & $s & " not known in `UnitTable`.")

proc getShort*(tab: UnitTable, s: string): DefinedUnit =
  result = tab.units[tab.short[s]]

proc `[]`*(tab: UnitTable, q: CTQuantity): DefinedUnit =
  let idx = tab.quantity[q.getName()]
  result = tab.units[idx]

proc `[]`*(tab: UnitTable, q: CTBaseQuantity): DefinedUnit =
  let idx = tab.quantity[q.name]
  result = tab.units[idx]

proc getUserDefined*(tab: UnitTable, s: string): UnitProduct =
  result = tab.userDefinedUnits[s]

proc getIdx*(tab: UnitTable, u: UnitInstance): int =
  ## Returns the index (i.e. priority) of the given unit instance
  let s = u.unit.name
  result = tab.long[s]

proc getIdx*(tab: UnitTable, s: string): int =
  ## Returns the index (i.e. priority) of the given unit instance
  result = tab.long[s]

iterator baseQuantities*(tab: UnitTable): CTBaseQuantity =
  for _, idx in tab.quantity:
    let unit = tab.units[idx]
    if unit.quantity.kind == qtFundamental:
      yield unit.quantity.b

iterator shortNames*(tab: UnitTable): string =
  for name, _ in tab.short:
    yield name

iterator longNames*(tab: UnitTable): string =
  for name, _ in tab.long:
    yield name

iterator longBaseNames*(tab: UnitTable): string =
  for name, _ in tab.longBaseUnits:
    yield name
