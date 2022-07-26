# stdlib
import std / [macros, tables, strutils, sets, algorithm, math, sequtils]
# local files
import core_types, quantities, ct_unit_types, parse_units, macro_utils

type
  DefinedUnits* = seq[DefinedUnit]

  ## XXX: Ideally we would replace the QuantityTab Table by a CacheTable. But then we need to
  ## use `newLit` to store a `CTQuantity` in it, which means on the "other side" (i.e. here)
  ## we need to again parse the AST into an object?

## The `UnitTab` is one of the fundamenal pieces of the macro logic. It stores all units
## defined in the `declareUnit` call and is later used to access type information.
var UnitTab {.compileTime.} = newUnitTable()

proc parseDefinedUnit*(x: string): UnitProduct =
  ## Overload that wraps the local `UnitTab` and hands it for parsing
  result = UnitTab.parseDefinedUnit(x)

proc parseDefinedUnit*(x: NimNode): UnitProduct =
  ## Overload that wraps the local `UnitTab` and hands it for parsing
  result = UnitTab.parseDefinedUnit(x)

proc `<`*(a, b: UnitInstance): bool =
  ## Comparison based on the order in `UnitTab`.
  let aIdx = UnitTab.getIdx(a)
  let bIdx = UnitTab.getIdx(b)
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

proc sorted*(u: UnitProduct): UnitProduct =
  ## Returns a unit sorted version of `u`
  result = newUnitProduct(u.value)
  result.units.sort()

proc toQuantityPower(units: UnitProduct): seq[QuantityPower] =
  ## Convert the given product of units to a `seq[QuantityPower]` such that
  ## that it encodes precisely the dimensionality of the unit.
  ##
  ## Note: if we had a strictly defined number of dimensions, we could in principle
  ## allocate a fixed size array where each index corresponds to a specific dimension.
  ## However, given that we are trying to be flexible in the definition of quantities
  ## even, this is not really feasible at the moment.
  ## TODO: It is worth a try though to see if we can refactor this further to have
  ## the `declareQuantities` macro generate such a thing for us, possibly by generating
  ## a `BaseQuantityKind` enum, which would allow us to write `array[BaseQuantityKind, int]`?
  ##
  ## This
  ##   var ar: array[QuantityKind, int]
  ##   for u in unit.units:
  ##     ar[u.toQuantityKind] = 2
  ## cannot work in the code here, as the `QuantityKind` enum isn't defined at CT of this
  ## procedure yet. The declaration would have to happen in a module that is imported
  ## by this one (i.e. inside `quantities`). But that is very problematic as then the quantities
  ## are "hard coded".
  ## Instead at least we could add an `id` to each `CTBaseQuantity` (and `CTQuantity`) such
  ## that we have an easier time to add / compare etc. quantities.

  # 1. compute the combined QuantityPowers of all units in the product
  var rawPowers = newSeq[QuantityPower]()
  for u in units.units:
    rawPowers.add u.toQuantityPower()
  # 2. simplify them by reducing same dimensions, add them to a CountTable
  #    with power as table argument
  var cTab = initCountTable[CTBaseQuantity]()
  for qp in rawPowers:
    inc(cTab, qp.quant, qp.power)
  # 3. compute result by placing back in a seq
  for (q, p) in pairs(cTab):
    result.add QuantityPower(quant: q, power: p)
  result.sort() # sort to make sure we get the same order

  when false:
    result = initTable[QuantityKind, int]()
    proc addQuant(res: var Table[QuantityKind, int], bu: BaseUnitKind, power: int) =
      let quantity = bu.toQuantity
      if quantity in res:
        res[quantity] += power
      else:
        res[quantity] = power
      if res[quantity] == 0:
        res.del(quantity)
    ## A comment, because at first glance this might seem confusing.
    ##
    ## We walk over the CTCompoundUnit and for every unit:
    ## - if a unit is not a compound unit (is a base unit) we simply add its quantity with the
    ##   power of the given unit to the table.
    ## - for compound units we have to be more careful: take the power of the unit (e.g. N² -> power 2)
    ##   and multiply with power of base units (e.g. Newton -> (kg•m•s⁻²)²). That's why we cannot just use
    ##   `only` the base units power or `only` the units power.
    for u in a.units:
      if not u.unitKind.isCompound:
        doAssert u.b.power == 1
        result.addQuant(u.b.baseUnit, u.power)
      else:
        for bu in u.bs:
          result.addQuant(bu.baseUnit, (u.power * bu.power))
    if result.len == 0:
      result[qkUnitLess] = 1

proc commonQuantity*(a, b: UnitProduct): bool =
  ## Comparison is done by checking for the same base units and powers using
  ## `UnitProduct`.
  let aQuant = a.toQuantityPower()
  let bQuant = b.toQuantityPower()
  result = aQuant == bQuant

proc commonQuantity*(x: typedesc, y: typedesc): bool =
  ## checks if x and y are equivalent quantities
  let xCT = x.getTypeInst.parseDefinedUnit()
  let yCT = y.getTypeInst.parseDefinedUnit()
  result = xCT.commonQuantity(yCT)

proc toNimType*(u: UnitInstance, short = false): string =
  #if u.unitKind == ukUnitLess: return
  ## XXX: handle base prefix of base units!
  # We use the prefix of the base unit exactly then, when the instance
  # does not override the prefix.
  let prefix = u.prefix # if u.prefix != siIdentity: u.prefix
               #else: u.unit.basePrefix
  let siPrefixStr = if short: SiShortPrefixTable[prefix]
                    else: SiPrefixTable[prefix]
  result = siPrefixStr
  if not short:
    result.add u.unit.name
  else:
    result.add u.unit.short
  if u.power < 0:
    result.add "⁻"
  if u.power > 1 or u.power < 0:
    for digit in getPow10Digits(u.power):
      result.add digits[digit]

proc toNimTypeStr*(x: UnitProduct, short = false): string =
  ## converts `x` to the correct string representation
  # return early if no units in x
  if x.units.len == 0: return "UnitLess"
  ## XXX: add a `reduce` call / simplify
  let xSorted = x.units.sorted
  for idx, u in xSorted:
    #if u.unitKind == ukUnitLess: continue
    var str = toNimType(u, short)
    if idx < xSorted.high:
      str.add "•"
    result.add str

proc toNimType*(x: UnitProduct, short = false): NimNode =
  ## converts `x` to the correct
  # return early if no units in x
  let name = x.toNimTypeStr(short)
  result = if name.len == 0: ident("UnitLess") else: ident(name)



proc toBaseTypeScale*(u: UnitInstance): float =
  result = u.prefix.toFactor()
  result *= u.value
  ## XXX: multiply the `conversion` factor possibly if unit is not a base unit
  case u.unit.kind
  of utBase: discard
  of utDerived:
    # multiply with conversion factor
    result *= u.unit.conversion.value
  # divide out any possible base prefixes (e.g. for kilo gram)
  result /= u.unit.basePrefix.toFactor()
  result = pow(result, u.power.float)

## TODO: better distinguish between converting to base SI prefixes and
## converting non SI units to SI?
proc toBaseTypeScale*(x: UnitProduct): float =
  ## returns the scale required to turn `x` to its base type, i.e.
  ## turn all units that are not already to its base form. This
  ## includes converting non base prefix units to their base prefixes
  ## *as well as* converting derived units (e.g. lbs, inch, ...) to
  ## their base unit counterparts.
  # XXX: ideally we could make sure `siPrefix` is init'd to 1.0
  result = if x.value != 0.0: x.value else: 1.0 # global SI prefix as a factor
  for u in x.units:
    result *= toBaseTypeScale(u)

proc toBaseType*(u: UnitInstance, needConversion: bool): UnitProduct =
  ## Returns a modified instance of this unit that is based on the
  ## base units of the system. E.g. `lbs` will be converted to `kg` assuming
  ## SI system.
  result = newUnitProduct()
  if not needConversion or not u.unit.autoConvert:
    ## If no auto conversion wished, simply assign base prefix (i.e. reset prefix) and add
    var mu = u
    mu.prefix = u.unit.basePrefix
    result.add mu
  else:
    case u.unit.kind
    of utBase:
      var mu = u
      # must assign `basePrefix` to get correct prefix in the *`UnitInstance`* and not just
      # `DefinedUnit` for units where `basePrefix` is non trivial (e.g. kg being base)
      mu.prefix = u.unit.basePrefix
      result.units.add mu
    of utDerived:
      # result is simply the conversion (factor + unit) of this derived unit
      result = u.unit.conversion
      # now add possible further prefixes / powers of the input
      result.value *= pow(u.prefix.toFactor(), u.power.float) # XXX: * u.value ? we don't use `value` atm

proc toBaseType*(x: UnitProduct, needConversion: bool): UnitProduct =
  ## converts `x` to a unit representing the base type.
  ## WARNING: this is a lossy conversion, so make sure to extract the
  ## conversion scales using `toBaseTypeScale` before doing this!
  ## TODO: can we add to `CTUnit` a scale?
  result = newUnitProduct()
  for u in x.units:
    result.add u.toBaseType(needConversion)

proc toBaseUnit(q: CTBaseQuantity): DefinedUnit =
  result = UnitTab[q]

proc toUnitInstance*(q: CTBaseQuantity): UnitInstance =
  let bUnit = q.toBaseUnit()
  result = newUnitInstance(q.name, bUnit, 1, bUnit.basePrefix)

proc toUnitInstance*(b: QuantityPower): UnitInstance =
  let bUnit = b.quant.toBaseUnit()
  result = newUnitInstance(b.quant.name, bUnit, b.power, bUnit.basePrefix)

proc toBaseUnits(q: CTQuantity): UnitProduct =
  ## Turns the given quantity into a `UnitProduct` of base units
  case q.kind
  of qtCompound:
    result = newUnitProduct()
    for b in q.baseSeq:
      let baseUnit = b.quant.toBaseUnit()
      let power = b.power
      result.units.add newUnitInstance(
        q.getName(), baseUnit, power, baseUnit.basePrefix
      )
  else: discard

proc flatten*(units: UnitProduct, needConversion = true): UnitProduct =
  ## extracts all base units from individual compound units and turns it into
  ## a single UnitProduct of only base units. Finally simplifies the result.
  ##
  ## It will flatten all compound units that have the `autoConvert` property
  ## set to `true`.
  result = newUnitProduct()
  var factor = 1.0
  for unitInst in units.units:
    let power = unitInst.power
    let prefix = unitInst.prefix
    let unit = unitInst.unit
    if not needConversion or not unitInst.unit.autoConvert:
      #var mu = unitInst
      #factor *= pow(prefix.toFactor(), power.float)
      #mu.prefix = mu.unit.basePrefix
      result.add unitInst
    else:
      # unit allows auto conversion
      case unit.quantity.kind
      of qtFundamental: result.add unitInst # fundamental quantities remain as they are
      of qtCompound:
        # compound quantities will be flattened
        ## TODO: how to handle global SI prefix in a compound CTUnit? Absorb
        ## into a `factor` on ``one`` of the new CTUnits to be added?
        ## For the purpose of `flatten` this does not play a role, because
        ## units of different SI prefixes are still equal? Well, but they aren't
        ## really. Can we do maths with them? Sure. Should they match in the
        ## concept `SomeUnit`? No! If `Meter•Second⁻¹` is demanded we need that and
        ## not allow `CentiMeter•Second⁻¹`?
        # conversion factor is SI prefix to the unit's power
        factor *= pow(prefix.toFactor, power.float)
        for b in unit.quantity.baseSeq:
          # convert given `QuantityPower` to a base unit
          var u = b.toUnitInstance()
          # and adjust power of this instance by power of the full compound
          u.power *= power
          #u.prefix =
          result.add u
        ## XXX: if derived (but autoConvert allowed) multiply by conversion
        # of utDerived:
  result.value = factor # assign the prefix

proc simplify*(x: UnitProduct, mergePrefixes = false): UnitProduct =
  ## simplifies the given unit `x`. E.g. turns `kg•kg` into `kg²`
  ##
  ## Note: this procedure expects to be called on *flattened* units, i.e.
  ## we do *not* check the compound nature of units, if there are any, but
  ## we only simplify what's here. We also *do not* handle different SI prefixes.
  ## They will be treated as separate units.
  ##
  ## WARNING: This is a lossy procedure in terms of conversion factors, if
  ## `mergePrefixes` is set to `true`. Make sure to call `toBaseTypeScale` before
  ## using.
  ## `mergePrefixes` keeps the prefix ``iff`` a unit only appears with one prefix.
  ## If a unit appears with multiple prefixes, the `DefinedUnit.basePrefix` is used
  ## instead (thus making it lossy).
  ##
  ## Further the `value` field of `x` (product of prefixes & conversions) will be *reset*
  ## so that we can deduce the conversion from the resulting type to its base type.
  # the `value` (product of prefixes & conversions) is *reset* so that this new
  # unit. Therefore do not assign `x.value` to `result.value`.
  result = newUnitProduct()
  if mergePrefixes:
    var cTab = initCountTable[DefinedUnit]()
    # prefixTab stores the prefixes of units we add
    var prefixTab = initTable[DefinedUnit, SiPrefix]()
    for u in x.units:
      if u.unit in prefixTab and prefixTab[u.unit] != u.prefix:
        # have different prefixes of the same unit, need to merge
        # to base prefix. This makes `mergePrefixes` lossy.
        prefixTab[u.unit] = u.unit.basePrefix
      else:                      # prefix only once so far, keep it
        prefixTab[u.unit] = u.prefix
      inc(cTab, u.unit, u.power) # ignore prefix, i.e. merge them all to one

    for unit, power in pairs(cTab):
      if power != 0: # power 0 implies unit is divided out
        result.add newUnitInstance($unit, unit, power, prefixTab[unit])
  else:
    var cTab = initCountTable[(DefinedUnit, SiPrefix)]()
    for u in x.units:
      inc(cTab, (u.unit, u.prefix), u.power) # treat prefixes as distinct
    for tup, power in pairs(cTab):
      let (unit, prefix) = tup
      if power != 0: # power 0 implies unit is divided out
        result.add newUnitInstance($unit, unit, power, prefix)

proc invert*(x: UnitProduct): UnitProduct =
  ## Inverts the given `UnitProduct`, i.e. it performs a "division".
  result = newUnitProduct()
  for u in x.units:
    var unit = u
    unit.power = -unit.power
    result.add unit

proc `==`*(a, b: UnitProduct): bool =
  ## comparison done by:
  ## - only equal if set of `unitKind` is same
  ## - only equal if for each element of `unitKind` set the `power` is the same
  ## - only equal if for each element the SiPrefix is the same
  ##
  ## Units are equal iff:
  ## - the product of all *base units* (including their power) is the same
  ## Since there are multiple representations of the same unit (e.g. `Newton` as
  ## a single UnitInstance or a `UnitProduct` comprising base units up to `Newton`)
  ## we have to flatten each input and then compare for same base units & powers.
  let aScale = a.toBaseTypeScale()
  let bScale = b.toBaseTypeScale()
  let aFlat = a.flatten.simplify
  let bFlat = b.flatten.simplify
  let aFlatSeq = aFlat.units.sorted
  let bFlatSeq = bFlat.units.sorted

  # to really make sure they are equal have to compare the si prefix of each
  if aFlatSeq.len != bFlatSeq.len:
    return false
  for idx in 0 ..< aFlatSeq.len:
    if aFlatSeq[idx] != bFlatSeq[idx]:
      return false
  if aScale != bScale:
    return false
  result = true

proc needConversionToBase*(a, b: UnitProduct): bool =
  ## Returns true, if the given products `a` and `b` contain compound units that
  ## represent the same quantity, but are of different units.
  # compare the (possible compound) quantities and see if they have different units
  let aSorted = a.sorted()
  let bSorted = a.sorted()
  result = true
  if a.len == b.len:
    # same length, just check if they have the same units (ignoring prefixes)
    result = false # if the loop matches, we *don't* need to convert
    for i in 0 ..< a.len:
      if a.units[i].unit != b.units[i].unit:
        return true
  elif a.len == 0 or b.len == 0:
    result = false # if one of them is UnitLess, no need to convert

## ##############################################################
## Code dealing with the `declareUnit` macro & parsing of its AST
## ##############################################################
proc parseShort(n: NimNode): string =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = n[0].strVal

proc parseQuantity(n: NimNode): CTQuantity =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = QuantityTab[n[0].strVal]

proc parseConversion(n: NimNode): UnitProduct = ## DefinedUnitValue !
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind in {nnkDotExpr}
  let dotEx = n[0]
  result = parseDefinedUnit(dotEx[1].strVal)
  case dotEx[0].kind
  of nnkIntLit: result.value = float(dotEx[0].intVal)
  of nnkFloatLit: result.value = dotEx[0].floatVal
  else: error("Invalid node for unit conversion definition " & $dotEx.repr & ".")

proc parsePrefix(n: NimNode): SiPrefix =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = parseEnum[SiPrefix](n[0].strVal)

proc parseAutoConvert(n: NimNode): bool =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = parseBool(n[0].strVal)

proc assertOption(n: NimNode): bool =
  result = n.kind == nnkCall and
    n.len == 2 and
    n[0].kind == nnkIdent and
    n[1].kind == nnkStmtList

proc parseUnit(tab: var UnitTable, n: NimNode): DefinedUnit =
  ## Parses:
  ##
  ##  Call
  ##    Ident "Meter"
  ##    StmtList
  ##      Call
  ##        Ident "short"
  ##        StmtList
  ##          Ident "m"
  ##      Call
  ##        Ident "quantity"
  ##        StmtList
  ##          Ident "Length"
  if not assertOption(n):
    error("Invalid field " & $n.repr & " in `declareUnits` definining a unit.")
  let name = n[0].strVal
  var
    short: string
    quantity: CTQuantity
    conversion = newUnitProduct()
    hasConversion = false
    prefix: SiPrefix = siIdentity # identity if not otherwise specified
    autoConvert = true
  for arg in n[1]: # parse the stmt list, i.e. the "options"
    if not assertOption(arg):
      error("Invalid field " & $arg.repr & " in `declareUnits` setting a unit option.")
    let field = arg[0].strVal
    case field
    of "short": short = parseShort(arg[1])
    of "quantity": quantity = parseQuantity(arg[1])
    of "conversion": hasConversion = true; conversion = parseConversion(arg[1])
    of "prefix": prefix = parsePrefix(arg[1])
    of "autoConvert": autoConvert = parseAutoConvert(arg[1])
    else: error("Invalid unit option " & $field & " defining the unit: " & $result.name & ".")

  if hasConversion:
    # is *not* a base unit (inch, Liter, PoundForce, ..., something that needs conversion)
    result = newDefinedUnit(utDerived, name, prefix, short, quantity, autoConvert,
                            quantity.kind, conversion = conversion)
  else:
    # *is* a base unit (but possibly compound!)
    result = newDefinedUnit(utBase, name, prefix, short, quantity, autoConvert,
                            quantity.kind)
  if quantity.kind == qtCompound and not hasConversion:
    tab.insert(result, hasConversion, quantity.toBaseUnits.toNimTypeStr())
  else:
    tab.insert(result, hasConversion)

proc parseUnits(tab: var UnitTable, n: NimNode): DefinedUnits =
  ## Handles parsing the given base units or derived units
  ##
  ##    StmtList
  ##      Call
  ##        Ident "Meter"
  ##        ...
  ##      Call
  ##        Ident "Second"
  ##        ...
  ##
  ## Parses the given calls defining the units.
  doAssert n.kind == nnkStmtList
  for unit in n:
    result.add tab.parseUnit(unit)

proc addNaturalUnitConversions(tab: var UnitTable, n: NimNode) =
  ## Parses the following tree and adjusts the `toNaturalUnit` conversion
  ## field for the referenced units.
  ## Call
  ##   Ident "Gram"
  ##   StmtList
  ##     DotExpr
  ##       FloatLit 1.7826627e-36
  ##       Ident "eV"
  doAssert n.kind == nnkStmtList
  for unit in n:
    doAssert unit.kind == nnkCall and unit[0].kind == nnkIdent and unit[1].kind == nnkStmtList
    let name = unit[0].strVal
    let body = unit[1][0]
    var conv: UnitProduct
    case body.kind
    of nnkDotExpr:
      doAssert body[0].kind == nnkFloatLit
      conv = parseDefinedUnit(body[1].strVal)
      conv.value = body[0].floatVal
    of nnkFloatLit:
      doAssert body.floatVal == 1.0
      conv = newUnitProduct()
    else:
      error("Invalid node kind " & $body.kind & " for definition of natural unit conversion " &
        "for unit: " & $name)
    let idx = tab.getIdx(name)
    # get the unit from the table, adjust the field and write back
    var definedUnit = tab.units[idx]
    doAssert definedUnit.quantityKind == qtFundamental, "Can only define natural unit conversions for base units!"
    definedUnit.toNaturalUnit = conv
    tab.units[idx] = definedUnit

proc parseCall(tab: var UnitTable, c: NimNode): DefinedUnits =
  ## Handles dispatching based on base units / derived ident
  ##
  ##  Call
  ##    Ident "BaseUnits" # or "Derived"
  ##    StmtList
  ##      Call
  ##        ...
  doAssert c.kind == nnkCall
  doAssert c.len == 2
  doAssert c[1].kind == nnkStmtList
  var baseUnits: DefinedUnits
  var derivedUnits: DefinedUnits
  if c[0].kind == nnkIdent and c[0].strVal == "BaseUnits":
    baseUnits = tab.parseUnits(c[1])
  elif c[0].kind == nnkIdent and c[0].strVal == "Derived":
    derivedUnits = tab.parseUnits(c[1]) ## FIXME: hand `baseUnits` to above procs to lookup conversions? baseUnits)
  elif c[0].kind == nnkIdent and c[0].strVal == "NaturalUnits":
    tab.addNaturalUnitConversions(c[1])
  else:
    error("The branch " & $c[0].repr & " is not known.")
  result.add baseUnits
  result.add derivedUnits

proc genUnitTypes(units: DefinedUnits): NimNode =
  ## Generates all unit type definitions.
  ##
  ## In the general case, e.g. for `Newton`:
  ##
  ##  type
  ##    # maybe this won't be needed anymore?
  ##    KiloGram•Meter•Second⁻²* = distinct Force
  ##    Newton* = KiloGram•Meter•Second⁻²
  ##    N* = Newton
  ##
  ## And for base units:
  ##
  ##  type
  ##    Meter* = distinct Length
  ##    m* = Meter
  ##
  ## This is because compound units are not necessarily unique.
  result = nnkTypeSection.newTree()
  # 1. generate all units for all compound quantities
  var generatedCompounds = initHashSet[string]()
  for quant in compoundQuantities(QuantityTab):
    let typ = quant.toBaseUnits().toNimTypeStr()
    let typShort = quant.toBaseUnits().toNimTypeStr(short = true)
    if typ notin generatedCompounds:
      result.add defineDistinctType(typ, quant.getName())
      result.add defineType(typShort, typ)
      generatedCompounds.incl typ

  for unit in units:
    let asBase = unit.quantity.toBaseUnits()
    if not asBase.isNil and unit.kind == utBase: # not a unit with a conversion
      # this is a base compound unit without a conversion
      # 1. generate the alias for the long name
      let compound = asBase.toNimTypeStr()
      result.add defineType(unit.name, compound)
      # 2. generate the short name alias
      result.add defineType(unit.short, unit.name)
    else:
      # 1. generate main definition, i.e. long name = distinct quantity
      result.add defineDistinctType(unit.name, unit.quantity.getName())
      # 2. generate the short name
      result.add defineType(unit.short, unit.name)

macro declareUnits*(defs: untyped): untyped =
  ## Declares a set of base and derived units.
  ##
  ## NOTE: the order in which the units are defined is their priority in terms of
  ## generated units. E.g. if you define `Gram` before `Meter`, then every auto generated
  ## unit will be written with `Gram•...•Meter•...•`.
  ##
  ## XXX: Currently we only generate a single compound quantity of one specific
  ## dimensionality (e.g. Second⁻¹ for both Frequency and Activity). The order depends
  ## on the definition of the quantities in the `declareQuantity` macro call.
  ##
  ##   This means that e.g. Becquerel is actually a `Frequency` now, as it is an
  ##   alias to Second⁻¹, which is `distinct Frequency` due to being first in the
  ##   macro call.... How can we better handle this? In practice this shouldn't really
  ##   matter though (it was the same before anyway)
  ##
  ## `autoConvert` can be used to set whether a unit will be converted to its base
  ## quantity representation (i.e. N -> kg•m•s⁻²) in a `flatten` call. This has effects
  ## for unit math with similar units.
  var units: DefinedUnits
  for call in defs:
    if call.kind == nnkCall:
      units.add UnitTab.parseCall(call)
    else:
      error("Invalid node kind " & $call.kind & " for `declareUnits` definition.")
  result = newStmtList()
  # 1. generate type definitions
  result.add genUnitTypes(units)

proc genSiPrefixes(n: NimNode, genShort: bool, genLong: bool,
                   excludes: seq[string] = @[]): seq[NimNode] =
  ## get the type of the unit so that we know what to base these units on
  let typ = n
  if genLong:
    for (si, prefix) in SiPrefixStringsLong:
      if prefix == siIdentity: continue
      if si in excludes:
        result.add ident("_") # if we exclude, add placeholder. Used to mark for cross reference long / short
        continue
      let typStr = ident($si & typ.strVal)
      let isTyp = nnkDistinctTy.newTree(typ)
      result.add nnkTypeDef.newTree(nnkPostfix.newTree(ident"*", typStr), newEmptyNode(), isTyp)
  if genShort:
    for (si, prefix) in SiPrefixStringsShort:
      if prefix == siIdentity: continue
      if si in excludes:
        result.add ident("_") # if we exclude, add placeholder. Used to mark for cross reference long / short
        continue
      let typStr = ident($si & typ.strVal)
      result.add typStr

macro generateSiPrefixedUnits*(units: untyped): untyped =
  ## generates all SI prefixed units for all units given. That just means
  ## appending each SI prefix to these units, both in long and short form.
  ## NOTE: This should only be used on ``raw`` units and not explicit compound
  ## units!
  expectKind(units, nnkStmtList)
  result = nnkTypeSection.newTree()
  for unit in units:
    doAssert unit.kind in {nnkTupleConstr, nnkPar, nnkCommand}
    var sisShort: seq[NimNode]
    var sisLong: seq[NimNode]
    if unit.kind == nnkCommand:
      var excludes: seq[string]
      doAssert unit[0].kind in {nnkTupleConstr, nnkPar}
      doAssert unit[1].kind == nnkCommand
      let excls = unit[1]
      doAssert excls[0].kind == nnkIdent and excls[0].strVal == "exclude"
      doAssert excls[1].kind == nnkBracket
      for br in excls[1]:
        doAssert br.kind == nnkIdent
        excludes.add br.strVal
      sisShort = genSiPrefixes(unit[0][0], true, false, excludes = excludes)
      sisLong = genSiPrefixes(unit[0][1], false, true, excludes = excludes)
    else:
      sisShort = genSiPrefixes(unit[0], true, false)
      sisLong = genSiPrefixes(unit[1], false, true)
    for si in sisLong:
      result.add si
    ## generate cross references from long to short
    let skipIdent = ident("_") # if a prefix is excluded, added as `_`. Thus skip those cross references
    for (siShort, siLong) in zip(sisShort, sisLong):
      if eqIdent(siShort, skipIdent) or eqIdent(siLong, skipIdent): continue
      result.add nnkTypeDef.newTree(nnkPostfix.newTree(ident"*", siShort), newEmptyNode(), siLong[0][1])