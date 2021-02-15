import math, macros, options, sequtils, algorithm, sets, tables, strutils, unicode, typetraits, strformat

type
  Unit* = distinct float

  Quantity* = distinct Unit
  CompoundQuantity* = distinct Quantity

  UnitLess* = distinct Unit

  ## Baes Quantities
  Time* = distinct Quantity
  Length* = distinct Quantity
  Mass* = distinct Quantity
  Current* = distinct Quantity
  Temperature* = distinct Quantity
  AmountOfSubstance* = distinct Quantity
  Luminosity* = distinct Quantity

  BaseQuantity* = Time | Length | Mass | Current | Temperature | AmountOfSubstance | Luminosity

  ## Derived quantities, TODO: should be `distinct CompoundQuantity`? Not a single dimension!
  Velocity* = distinct CompoundQuantity
  Acceleration* = distinct CompoundQuantity
  Momentum* = distinct CompoundQuantity
  Force* = distinct CompoundQuantity
  Energy* = distinct CompoundQuantity
  Density* = distinct CompoundQuantity

  ElectricPotential* = distinct CompoundQuantity
  Voltage* = ElectricPotential

  Frequency* = distinct CompoundQuantity

  Charge* = distinct CompoundQuantity
  Power* = distinct CompoundQuantity
  ElectricResistance* = distinct CompoundQuantity
  Capacitance* = distinct CompoundQuantity
  Inductance* = distinct CompoundQuantity
  Pressure* = distinct CompoundQuantity

  DerivedQuantity* = Velocity | Acceleration | Momentum | Force | Energy | Density | ElectricPotential | Voltage |
    Frequency | Charge | Power | ElectricResistance | Capacitance | Inductance | Pressure

  SomeQuantity* = BaseQuantity | DerivedQuantity

  #Joule* = distinct Unit
  ## Base SI units
  Second* = distinct Time
  Meter* = distinct Length
  Gram* = distinct Mass
  KiloGram* = distinct Gram ## KiloGram is special due to being the actual SI unit. Thus defined here
  Ampere* = distinct Current
  Kelvin* = distinct Temperature
  Mol* = distinct AmountOfSubstance
  Candela* = distinct Luminosity

  SiUnit* = Second | Meter | KiloGram | Ampere | Kelvin | Mol | Candela

  ## compound units, i.e. definition of different physical concepts.
  KiloGram•Meter•Second⁻¹* = distinct Momentum
  Second²* = distinct Time
  Meter•Second⁻¹* = distinct Velocity
  Meter•Second⁻²* = distinct Acceleration
  KiloGram•Meter²•Second⁻²* = distinct Energy
  KiloGram•Meter•Second⁻²* = distinct Force
  Second⁻¹* = distinct Frequency
  KiloGram•Meter²•Second⁻³* = distinct Power
  Ampere•Second* = distinct Charge
  KiloGram•Meter²•Second⁻²•Ampere⁻²* = distinct Inductance
  Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹* = distinct Capacitance
  KiloGram•Meter²•Ampere⁻¹•Second⁻³* = distinct ElectricPotential
  KiloGram•Meter²•Second⁻³•Ampere⁻²* = distinct ElectricResistance
  KiloGram•Meter⁻¹•Second⁻² = distinct Pressure
  KiloGram•Meter⁻³ = distinct Density

  ## derived SI units
  Newton* = KiloGram•Meter•Second⁻²
  Joule* = KiloGram•Meter²•Second⁻²
  Volt* = KiloGram•Meter²•Ampere⁻¹•Second⁻³
  Hertz* = Second⁻¹
  Coulomb* = Ampere•Second
  Watt* = KiloGram•Meter²•Second⁻³ # Joule•Second⁻¹
  Ohm* = KiloGram•Meter²•Second⁻³•Ampere⁻²
  Henry* = KiloGram•Meter²•Second⁻²•Ampere⁻²
  Farad* = Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹
  Pascal* = KiloGram•Meter⁻¹•Second⁻²

  ## other units
  ElectronVolt* = distinct Energy
  Bar* = distinct Pressure

  ## possibly define convenient overloads? Not really required, since we compute that these match after
  ## all, no? E.g. given Joule•Coulomb⁻¹. We would parse each, convert to base SI units and notice that
  ## it's the same as required `Volt` after conversion to base SI units for V. That's how it should work
  ## anyway.
  Joule•Coulomb⁻¹* = Volt
  Ampere•Ohm* = Volt

  DerivedSiUnits* = Newton | Joule

  ## shorthand types
  m* = Meter
  s* = Second
  m•s⁻²* = Meter•Second⁻²
  meterPerSecondSquared* = Meter•Second⁻²
  g* = Gram
  Kg* = KiloGram
  kg* = Kg
  N* = Newton
  V* = Volt
  Hz* = Hertz
  J* = Joule
  C* = Coulomb
  W* = Watt
  Ω* = Ohm
  H* = Henry
  F* = Farad
  eV* = ElectronVolt
  Pa* = Pascal
  bar* = Bar
  g•cm⁻³* = distinct Density

  SiPrefix* = enum
    siYocto, siZepto, siAtto, siFemto, siPico, siNano, siMicro, siMilli, siCenti, siDeci,
    siIdentity,
    siDeca, siHecto, siKilo, siMega, siGiga, siTera, siPeta, siExa, siZetta, siYotta

  QuantityKind* = enum
    # base quantities
    qkUnitLess, qkMass, qkLength, qkTime, qkCurrent, qkTemperature, qkAmountOfSubstance, qkLuminosity,
    # derived quantities
    qkFrequency, qkVelocity, qkAcceleration, qkMomentum, qkForce, qkEnergy, qkElectricPotential,
    qkCharge, qkPower, qkElectricResistance, qkInductance, qkCapacitance, qkPressure, qkDensity

  ## enum storing all known units (their base form) to allow easier handling of unit conversions
  ## Enum value is the default name of the unit
  UnitKind* = enum
    ukUnitLess = "UnitLess"
    ukGram = "Gram"
    ukMeter = "Meter"
    ukSecond = "Second"
    ukAmpere = "Ampere"
    ukKelvin = "Kelvin"
    ukMol = "Mol"
    ukCandela = "Candela"
    # derived SI units
    ukNewton = "Newton"
    ukJoule = "Joule"
    ukVolt = "Volt"
    ukHertz = "Hertz"
    ukCoulomb = "Coulomb"
    ukWatt = "Watt"
    ukOhm = "Ohm"
    ukHenry = "Henry"
    ukFarad = "Farad"
    ukPascal = "Pascal"
    ukBar = "Bar"
    # other units
    ukElectronVolt = "ElectronVolt"
    # natural units
    ukNaturalLength = "NaturalLength" # length
    ukNaturalMass = "NaturalMass" # mass
    ukNaturalTime = "NaturalTime" # time
    ukNaturalEnergy = "NaturalEnergy" # energy
    # ...
    # additional units
    ukPound = "Pound" # lbs (lb singular is too uncommon)
    ukInch = "Inch" # in ( or possibly "inch" due to in being keyword)
    ukMile = "Mile"
    # ...

  BaseUnitKind* = enum
    buUnitLess = "UnitLess"
    buGram = "Gram"
    buMeter = "Meter"
    buSecond = "Second"
    buAmpere = "Ampere"
    buKelvin = "Kelvin"
    buMol = "Mol"
    buCandela = "Candela"

  UnitType* = enum
    utQuantity, utCompoundQuantity

  CTBaseUnit* = object
    baseUnit: BaseUnitKind # what is the base unit of that quantity?
    power: int
    siPrefix: SiPrefix

  ## M
  CTUnit* = object # compile time object that stores a unit
    name: string
    isShortHand: bool ## stores if the given unit used shorthand `m` or verbose `Meter`
    ## TODO: can factor be a procedure? Then when generating code, we simply use `getAst` to
    ## get the body of the conversion and replace `x -> body(x)` inline?
    factor: float # stores possible conversion factors from converting to this unit
    unitKind: UnitKind # what unit is it?
    quantity: QuantityKind # what quantity does this unit refer to?
    power: int ## each unit needs a power
    siPrefix: SiPrefix ## and si prefix already! `km²` etc
    case unitType: UnitType
    of utQuantity:
      ## TODO: base unit can be takes from a mapping of `UnitKind -> BaseUnitKind`!
      b: CTBaseUnit
    of utCompoundQuantity: # in case `quantity` refers to a compound quantity, e.g. Newton
      ## one compound quantity is made up of multiple base units.
      bs: seq[CTBaseUnit]

  ## unit conversion can be done by walking over all units, checking for a
  ## unit containing BaseUnit, and converting using SiPrefix (use enum with values for SiPrefix?)
  ## and power of the unit
  ## In a way it might still make sense to keep CTCompoundUnit around. That way we can easier store
  ## things like `lbs * mile`, `N * m` etc without having to convert everything to its base units
  ## always?
  CTCompoundUnit* = object
    #value: Option[float]
    units: seq[CTUnit]

## parseing CT units is the basis of all functionality almost
proc parseCTUnit(x: NimNode): CTCompoundUnit
proc toNimType(x: CTCompoundUnit): NimNode
proc toNimType(u: CTUnit): string
proc flatten(units: CTCompoundUnit): CTCompoundUnit
proc simplify(x: CTCompoundUnit): CTCompoundUnit

proc pretty(x: CTUnit): string = x.toNimType()
proc pretty(x: CTCompoundUnit): string = x.toNimType().strVal

proc enumerateTypesImpl*(t: NimNode): NimNode =
  result = nnkBracket.newTree()
  for ch in t.getTypeImpl[1].getType:
    if ch.strVal == "or": continue
    result.add newLit(ch.strVal)

macro enumerateTypes(t: typed): untyped =
  result = enumerateTypesImpl(t)

macro quantityList*(): untyped =
  result = enumerateTypesImpl(bindSym("SomeQuantity"))
  result.add newLit"Quantity"
  result.add newLit"CompoundQuantity"

const qTypes* = quantityList()

proc resolveAlias(n: NimNode): NimNode =
  ## returns the first type that is `distinct` (i.e. convert Newton -> KiloGram•Meter•Second⁻²)
  case n.kind
  of nnkDistinctTy: result = n
  of nnkBracketExpr: result = (n[1].getImpl).resolveAlias
  of nnkSym:
    if n.getTypeInst != n:
      result = n.getTypeInst.resolveAlias
    else:
      result = n.getImpl.resolveAlias
  of nnkTypeDef:
    if n[2].kind == nnkDistinctTy: result = n[0]
    else: result = n[2].getImpl.resolveAlias
  else: result = newEmptyNode()

macro isAUnit*(x: typed): untyped =
  let x = x.resolveAlias()
  case x.kind
  of nnkSym:
    let typ = x
    var xT = if typ.kind == nnkDistinctTy: typ[2] else: typ
    while xT.strVal notin quantityList:
      xT = xT.getTypeImpl
      case xT.kind
      of nnkDistinctTy:
        xT = xT[0]
      else:
        return newLit false
  else:
    return newLit false
  ## in this case investigation is true
  result = newLit true

type
  SomeUnit* = concept x
    isAUnit(x)

proc `$`*[T: SomeUnit](s: T): string = &"{s.float:.4g} {$type(s)}"

when false:
  ## declare conversions. Defines mappings from x -> y that we store internally as:
  ## `Table[CTUnit, CTUnit]` where the input CTUnit is constructed with `toCTUnit`, i.e.
  ## has factor, power = 1, prefix siIdentity.
  ## If the conversion is both ways using `<->` ⇔ a tuple is required, which describes
  ## the conversions in both directions.
  ## Left: conversion from left -> right, Right: conversion from right -> left.
  ## If only a single conversion is given using `->` the inverse is assumed to be
  ## just the inverse of the given factor (i.e. has to be a float literal).
  ## Conversions listed here are required for all units that describe the same quantity!
  declareConversions:
    lbs -> kg: 0.45359237
    inch -> cm: 2.54
    mile -> km: 1.6
    Fahrenheit <-> Celsius: (5.0/9.0 * (x - 32), 9.0/5.0 * x + 32.0)

const digits = ["⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]
const digitsAndMinus = ["⁻","⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]

const SiPrefixStringsLong = {
  "Yocto" :    siYocto,
  "Zepto" :    siZepto,
  "Atto" :     siAtto,
  "Femto" :    siFemto,
  "Pico"  :    siPico,
  "Nano" :     siNano,
  "Micro" :    siMicro,
  "Milli" :    siMilli,
  "Centi" :    siCenti,
  "Deci" :     siDeci,
  "" : siIdentity,
  "Deca" :     siDeca,
  "Hecto" :    siHecto,
  "Kilo" :     siKilo,
  "Mega" :     siMega,
  "Giga" :     siGiga,
  "Tera" :     siTera,
  "Peta" :     siPeta,
  "Exa" :      siExa,
  "Zetta" :    siZetta,
  "Yotta" :    siYotta
}

const SiPrefixStringsShort = {
  "y"  :    siYocto,
  "z"  :    siZepto,
  "a"  :     siAtto,
  "f"  :    siFemto,
  "p"  :    siPico,
  "n"  :     siNano,
  "μ"  :    siMicro,
  "m"  :    siMilli,
  "c"  :    siCenti,
  "d"  :     siDeci,
  ""   : siIdentity,
  "da" :     siDeca,
  "h"  :    siHecto,
  "k"  :     siKilo,
  "M"  :     siMega,
  "G"  :     siGiga,
  "T"  :     siTera,
  "P"  :     siPeta,
  "E"  :      siExa,
  "Z"  :    siZetta,
  "Y"  :    siYotta
}

const SiPrefixTable = block:
  var tab = initTable[SiPrefix, string]()
  for (key, val) in SiPrefixStringsLong:
    tab[val] = key
  tab

proc genSiPrefixes(n: NimNode, genShort: bool, genLong: bool): seq[NimNode] =
  ## get the type of the unit so that we know what to base these units on
  let typ = n
  if genLong:
    for (si, prefix) in SiPrefixStringsLong:
      if prefix == siIdentity: continue
      let typStr = ident($si & typ.strVal)
      if typStr.strVal == "KiloGram": continue # skip generation of `KiloGram`
      let isTyp = nnkDistinctTy.newTree(typ)
      result.add nnkTypeDef.newTree(nnkPostfix.newTree(ident"*", typStr), newEmptyNode(), isTyp)
  if genShort:
    for (si, prefix) in SiPrefixStringsShort:
      if prefix == siIdentity: continue
      let typStr = ident($si & typ.strVal)
      if typStr.strVal == "kg": continue # predefined as well to have same number of elements in this seq
      result.add typStr

macro generateSiPrefixedUnits*(units: untyped): untyped =
  ## generates all SI prefixed units for all units given. That just means
  ## appending each SI prefix to these units, both in long and short form.
  ## NOTE: This should only be used on ``raw`` units and not explicit compound
  ## units!
  expectKind(units, nnkStmtList)
  result = nnkTypeSection.newTree()
  for unit in units:
    expectKind(unit, nnkPar)
    let sisShort = genSiPrefixes(unit[0], true, false)
    let sisLong = genSiPrefixes(unit[1], false, true)
    for si in sisLong:
      result.add si
    ## generate cross references from long to short
    for (siShort, siLong) in zip(sisShort, sisLong):
      result.add nnkTypeDef.newTree(nnkPostfix.newTree(ident"*", siShort), newEmptyNode(), siLong[0][1])

generateSiPrefixedUnits:
  (m, Meter)
  (s, Second)
  (g, Gram)
  (N, Newton)
  (V, Volt)
  (Hz, Hertz)
  (J, Joule)
  (C, Coulomb)
  (W, Watt)
  (Ω, Ohm)
  (H, Henry)
  (F, Farad)
  (eV, ElectronVolt)
  (Pa, Pascal)
  (bar, Bar)

proc isUnitLess(u: CTCompoundUnit): bool = u.units.len == 0

proc isCompound(unitKind: UnitKind): bool =
  result = unitKind notin {ukUnitLess .. ukCandela,
                           ukNaturalLength .. ukNaturalTime,
                           ukPound .. ukMile}

proc toQuantity(unitKind: UnitKind): QuantityKind =
  ## SI units
  case unitKind
  of ukUnitLess: result = qkUnitLess
  of ukSecond: result = qkTime
  of ukMeter: result = qkLength
  of ukGram: result = qkMass
  #of ukKiloGram: result = qkMass
  of ukAmpere: result = qkCurrent
  of ukKelvin: result = qkTemperature
  of ukMol: result = qkAmountOfSubstance
  of ukCandela: result = qkLuminosity
  # derived SI units
  of ukNewton: result = qkForce
  of ukJoule: result = qkEnergy
  of ukVolt: result = qkElectricPotential
  of ukHertz: result = qkFrequency
  of ukCoulomb: result = qkCurrent
  of ukWatt: result = qkPower
  of ukOhm: result = qkElectricResistance
  of ukHenry: result = qkInductance
  of ukFarad: result = qkCapacitance
  of ukPascal: result = qkPressure
  of ukBar: result = qkPressure
  # natural units
  of ukNaturalLength: result = qkLength
  of ukNaturalMass: result = qkMass
  of ukNaturalTime: result = qkTime
  of ukNaturalEnergy: result = qkEnergy
  # other units
  of ukElectronVolt: result = qkEnergy
  of ukPound: result = qkMass
  of ukInch: result = qkLength
  of ukMile: result = qkLength

proc toBaseUnit(unitKind: UnitKind): BaseUnitKind =
  ## SI units
  case unitKind
  of ukUnitLess: result = buUnitLess
  of ukSecond: result = buSecond
  of ukMeter: result = buMeter
  of ukGram: result = buGram
  #of ukKiloGram: result = buGram
  of ukAmpere: result = buAmpere
  of ukKelvin: result = buKelvin
  of ukMol: result = buMol
  of ukCandela: result = buCandela
  # natural units
  of ukNaturalLength: result = buMeter
  of ukNaturalMass: result = buGram
  of ukNaturalTime: result = buSecond
  # other units
  of ukPound: result = buGram
  of ukInch: result = buMeter
  of ukMile: result = buMeter
  else: error("Conversion to base unit not possible for compound units: " & $unitKind & "!")

  when false:
    # derived SI units
    case unitKind
    of ukNewton: result = ukForce
    of ukJoule: result = ukEnergy
    of ukVolt: result = ukElectricPotential
    of ukHertz: result = ukFrequency
    of ukCoulomb: result = ukCurrent
    of ukWatt: result = ukPower
    of ukOhm: result = ukElectricResistance
    of ukHenry: result = ukInductance
    of ukFarad: result = ukCapacitance
    # natural units
    of ukNaturalEnergy: result = ukEnergy

proc toCTBaseUnit(unitKind: UnitKind, power = 1, siPrefix = siIdentity): CTBaseUnit =
  doAssert not unitKind.isCompound, "Invalid call to `toBaseUnit` for compound unit `" & $unitKind & "`!"
  result = CTBaseUnit(baseUnit: unitKind.toBaseUnit(),
                      power: power,
                      siPrefix: siPrefix) #if unitKind == ukKiloGram: siKilo else: siIdentity)

proc toCTBaseUnitSeq(unitKind: UnitKind): seq[CTBaseUnit] =
  doAssert unitKind.isCompound, "Invalid call to `toBaseUnitSeq` for non compound unit `" & $unitKind & "`!"
  ## derived SI units
  case unitKind
  of ukNewton:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter)
    result.add toCTBaseUnit(ukSecond, power = -2)
  of ukJoule:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter, power = 2)
    result.add toCTBaseUnit(ukSecond, power = -2)
  of ukVolt:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter, power = 2)
    result.add toCTBaseUnit(ukAmpere, power = -1)
    result.add toCTBaseUnit(ukSecond, power = -3)
  of ukHertz:
    result.add toCTBaseUnit(ukSecond, power = -1)
  of ukCoulomb:
    result.add toCTBaseUnit(ukAmpere)
    result.add toCTBaseUnit(ukSecond)
  of ukWatt:
    result = toCTBaseUnitSeq(ukJoule)
    result.add toCTBaseUnit(ukSecond, power = -1)
  of ukHenry:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter, power = 2)
    result.add toCTBaseUnit(ukAmpere, power = -2)
    result.add toCTBaseUnit(ukSecond, power = -2)
  of ukOhm:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter, power = 2)
    result.add toCTBaseUnit(ukAmpere, power = -2)
    result.add toCTBaseUnit(ukSecond, power = -3)
  of ukFarad:
    result.add toCTBaseUnit(ukSecond, power = 4)
    result.add toCTBaseUnit(ukAmpere, power = 2)
    result.add toCTBaseUnit(ukMeter, power = -2)
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo, power = -1)
  of ukPascal:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukMeter, power = -1)
    result.add toCTBaseUnit(ukSecond, power = -2)
  of ukBar:
    result.add toCTBaseUnitSeq(ukPascal)
  of ukElectronVolt:
    ## our logic is incomplete, since it's missing proper conversions ouside of SI prefixes!
    result.add toCTBaseUnitSeq(ukJoule)
  # natural units
  of ukNaturalEnergy:
    result.add toCTBaseUnitSeq(ukJoule)
  else: error("Non compound unit `" & $unitKind & "` cannot be converted to sequence of CTBaseUnit!")

proc getConversionFactor(unitKind: UnitKind): float =
  case unitKind
  of ukElectronVolt: result = 1.602176634e-19 # relative to: Joule
  of ukPound: result = 0.45359237 # relative to: kg
  of ukMile: result = 1600 # relative to: m
  of ukInch: result = 0.0254 # relative to: cm
  of ukBar: result = 100_000 # relative to Pa
  else: result = 1.0

proc toCTUnit(unitKind: UnitKind): CTUnit {.compileTime.} =
  if not unitKind.isCompound:
    var baseUnit: CTBaseUnit
    if unitKind == ukGram:
      baseUnit = unitKind.toCTBaseUnit(siPrefix = siKilo)
    else:
      baseUnit = unitKind.toCTBaseUnit()
    result = CTUnit(name: $unitKind, factor: getConversionFactor(unitKind),
                    power: 1, siPrefix: siIdentity,
                    unitKind: unitKind,
                    quantity: unitKind.toQuantity(),
                    unitType: utQuantity,
                    b: baseUnit)
  else:
    ## convert to base units
    result = CTUnit(name: $unitKind, factor: getConversionFactor(unitKind),
                    unitKind: unitKind,
                    power: 1, siPrefix: siIdentity,
                    quantity: unitKind.toQuantity(),
                    unitType: utCompoundQuantity,
                    bs: toCTBaseUnitSeq(unitKind))

proc initCTUnit(name: string, unitKind: UnitKind, power: int, siPrefix: SiPrefix,
                isShortHand = false): CTUnit =
  result = unitKind.toCTUnit()
  result.name = name
  result.power = power
  result.siPrefix = siPrefix #if siPrefix == siIdentity and unitKind == ukGram: siKilo else: siPrefix
  result.isShortHand = isShortHand

#proc resolveToBaseType(n: NimNode):

## auto conversion of `UnitLess` to `float` is possible so that e.g. `sin(5.kg / 10.kg)` works as expected!
converter toFloat*(x: UnitLess): float = x.float
converter toUnitLess*(x: SomeNumber): UnitLess = x.UnitLess

macro defUnit*(arg: untyped): untyped =
  ## Helper template to define new units (not required to be used manually)
  let argCT = parseCTUnit(arg)
  let shortHand = argCT.units.allIt(it.isShortHand)

  ## TODO: instead of just using the long version, what to do for
  ## J•m or something like this? For max compatibility the RHS
  ## should actually be the base unit stuff.
  if shortHand:
    let resType = argCT.flatten.simplify.toNimType()
    result = quote do:
      when not declared(`resType`):
        type `resType` = distinct CompoundQuantity
      when not declared(`arg`):
        type `arg` = `resType`
  else:
    result = quote do:
      when not declared(`arg`):
        type `arg` = distinct CompoundQuantity
  echo result.repr

proc add(comp: var CTCompoundUnit, unit: CTUnit) =
  comp.units.add unit

proc add(comp: var CTCompoundUnit, unit: CTBaseUnit) =
  comp.units.add initCTUnit("", UnitKind(ord(unit.baseUnit)), unit.power, unit.siPrefix)

proc add(comp: var CTCompoundUnit, toAdd: CTCompoundUnit) =
  ## adding a sequence of compound units equates to multiplying units
  for u in toAdd.units:
    comp.add u

proc toSet(s: seq[CTUnit]): set[UnitKind] =
  for x in s:
    result.incl x.unitKind
    when false:
      case x.unitType
      of utQuantity: result.incl x.b.baseUnit
      of utCompoundQuantity:
        for xb in x.bs:
          result.incl xb.baseUnit

proc toHashSet(s: CTCompoundUnit): HashSet[tuple[unitKind: UnitKind, siPrefix: SiPrefix]] =
  result = initHashSet[tuple[unitKind: UnitKind, siPrefix: SiPrefix]]()
  for x in s.units:
    result.incl (unitKind: x.unitKind, siPrefix: x.siPrefix)

proc powerOfKind(s: seq[CTUnit], ukKind: UnitKind): int =
  for x in s:
    if x.unitKind == ukKind:
      result += x.power

proc powerOfKindAndPrefix(s: seq[CTUnit], ukKind: UnitKind, siPrefix: SiPrefix): int =
  for x in s:
    if x.unitKind == ukKind and x.siPrefix == siPrefix:
      result += x.power

proc simplify(x: CTCompoundUnit): CTCompoundUnit =
  ## simplifies the given unit `x`. E.g. turns `kg•kg` into `kg²`
  ## TODO: Add option to also simplify between different SI prefixes
  let xSet = x.toHashSet
  for el in xSet:
    let power = x.units.powerOfKindAndPrefix(el.unitKind, el.siPrefix)
    if abs(power) > 0:
      result.add initCTUnit("", el.unitKind, power, el.siPrefix)

proc invert(x: CTCompoundUnit): CTCompoundUnit =
  for u in x.units:
    var unit = u
    case u.unitType
    of utQuantity:
      unit.power = -unit.power
      unit.b.power = -unit.b.power
      result.add unit
    of utCompoundQuantity:
      unit.power = -unit.power
      for b in mitems(unit.bs):
        b.power = -b.power
      result.add unit

proc `==`(a, b: CTUnit): bool =
  result = (a.unitKind == b.unitKind and a.power == b.power and a.siPrefix == b.siPrefix)

proc `<`(a, b: CTUnit): bool =
  if a.unitKind < b.unitKind:
    result = true
  elif a.unitKind > b.unitKind:
    result = false
  else:
    if a.power < b.power:
      result = true
    elif a.power > b.power:
      result = false
    else:
      result = a.siPrefix < b.siPrefix

proc flatten(units: CTCompoundUnit): CTCompoundUnit =
  ## extracts all base units from individual compound CTUnits and
  ## turns it into a single CTCompoundUnit of only base units. Finally
  ## simplifies the result.
  for u in units.units:
    case u.unitType
    of utQuantity: result.add u
    of utCompoundQuantity:
      ## TODO: how to handle global SI prefix in a compound CTUnit? Absorb
      ## into a `factor` on ``one`` of the new CTUnits to be added?
      ## For the purpose of `flatten` this does not play a role, because
      ## units of different SI prefixes are still equal? Well, but they aren't
      ## really. Can we do maths with them? Sure. Should they match in the
      ## concept `SomeUnit`? No! If `Meter•Second⁻¹` is demanded we need that and
      ## not allow `CentiMeter•Second⁻¹`?
      let power = u.power
      let prefix = u.siPrefix
      for b in u.bs:
        var mb = b
        mb.power = mb.power * power
        # prefix handle ?
        result.add mb

proc commonQuantity(a, b: CTCompoundUnit): bool =
  ## comparison done by:
  ## - only equal if set of `baseUnit` is same
  ## - only equal if for each element of `baseUnit` set the `power` is the same
  let aFlat = a.flatten.toBaseType().simplify.units.sorted
  let bFlat = b.flatten.toBaseType().simplify.units.sorted
  # to really make sure they are equal have to compare the si prefix of each
  if aFlat.len != bFlat.len:
    return false
  for idx in 0 ..< aFlat.len:
    if aFlat[idx].quantity != bFlat[idx].quantity or aFlat[idx].power != bFlat[idx].power:
      return false
  result = true

proc `==`(a, b: CTCompoundUnit): bool =
  ## comparison done by:
  ## - only equal if set of `unitKind` is same
  ## - only equal if for each element of `unitKind` set the `power` is the same
  ## - only equal if for each element the SiPrefix is the same
  ##
  ## Units are equal iff:
  ## - the product of all *base units* (including their power) is the same
  ## Since there are multiple representations of the same unit (e.g. `Newton` as
  ## a single CTUnit or a `CTCompoundUnit` comprising base units up to `Newton`)
  ## we have to flatten each input and then compare for same base units & powers.
  let aFlat = a.flatten.simplify.units.sorted
  let bFlat = b.flatten.simplify.units.sorted
  # to really make sure they are equal have to compare the si prefix of each
  if aFlat.len != bFlat.len:
    return false
  for idx in 0 ..< aFlat.len:
    if aFlat[idx] != bFlat[idx]:
      return false
  result = true

macro `==`*[T: SomeUnit](a, b: T): bool =
  let xCT = parseCTUnit(a)
  let yCT = parseCTUnit(b)
  let units = xCT == yCT
  if not units:
    result = newLit false
  else:
    result = quote do:
      `a`.float == `b`.float

iterator getPow10Digits(x: int): int =
  ## yields all digits in given integer
  var digits: seq[int]
  var val = abs(x)
  while val > 0:
    digits.add val mod 10
    val = val div 10
  for el in digits.reversed:
    yield el

proc toNimType(u: CTUnit): string =
  let siPrefixStr = SiPrefixTable[u.siPrefix]
  result = siPrefixStr
  result.add $u.unitKind
  if u.power < 0:
    result.add "⁻"
  if u.power > 1 or u.power < 0:
    for digit in getPow10Digits(u.power):
      result.add digits[digit]

proc toNimType(x: CTCompoundUnit): NimNode =
  ## converts `x` to the correct
  # return early if no units in x
  if x.units.len == 0: return ident"UnitLess"
  let xSorted = x.units.sorted
  var name = ""
  for idx, u in xSorted:
    var str = toNimType(u)
    if idx < xSorted.high:
      str.add "•"
    name.add str
  result = ident(name)

proc parseUntil(s: string, chars: openArray[string]): int =
  ## parses until one of the runes in `chars` is found
  var idx = 0
  var rune: Rune
  var oldIdx = idx
  while idx < s.len:
    oldIdx = idx
    fastRuneAt(s, idx, rune)
    if rune.toUtf8 in chars:
      return oldIdx
  when false:
    for rune in utf8(s):
      if rune in chars:
        return idx
      inc idx
  # didn't find it
  result = -1

proc isLongBaseUnit(s: string): bool =
  for b in BaseUnitKind:
    if s.startsWith($b): return true

proc parseSiPrefix(s: var string): SiPrefix =
  ## Return early if we only have on rune in total or until we reach
  ## a `⁻` or any of the superscript numbers. Important for things like
  ## `m`. Only a prefix if there's more than one rune.
  if s.runeLen == 1 or s.runeAt(1).toUtf8() in digitsAndMinus: return siIdentity
  result = siIdentity
  if s.runeAt(0).isUpper:
    if s.isLongBaseUnit: return siIdentity # if it's Meter, Gram etc.
    ## try to find Long Si prefix
    for (el, prefix) in SiPrefixStringsLong:
      if prefix == siIdentity: continue
      if s.startsWith(el):
        s.removePrefix(el)
        return prefix
  ## no is upper does not mean it might not short, since some are upper
  ## else check for short prefix.
  for (el, prefix) in SiPrefixStringsShort:
    ## NOTE: we store kg internally using g even though it's not the SI unit!
    if prefix == siIdentity: continue
    ## TODO: properly fix this!!
    if prefix == siPeta and s.startsWith("Pound"): return siIdentity
    if prefix == siExa and s.startsWith("ElectronVolt"): return siIdentity
    if prefix == siMilli and s.startsWith("mol"): return siIdentity
    if prefix == siMega and s.startsWith("Mol"): return siIdentity
    if s.startsWith(el):
      s.removePrefix(el)
      return prefix

proc toFactor(prefix: SiPrefix): float =
  ## note: can't compute value reasonably, due to hecto, centi, deci and deca
  case prefix
  of siYocto: result = 1e-24
  of siZepto: result = 1e-21
  of siAtto: result = 1e-18
  of siFemto: result = 1e-15
  of siPico: result = 1e-12
  of siNano: result = 1e-9
  of siMicro: result = 1e-6
  of siMilli: result = 1e-3
  of siCenti: result = 1e-2
  of siDeci: result = 1e-1
  of siIdentity: result = 1.0
  of siDeca: result = 1e1
  of siHecto: result = 1e2
  of siKilo: result = 1e3
  of siMega: result = 1e6
  of siGiga: result = 1e9
  of siTera: result = 1e12
  of siPeta: result = 1e15
  of siExa: result = 1e18
  of siZetta: result = 1e21
  of siYotta: result = 1e24

proc hasNegativeExp(s: var string): bool =
  var rune: Rune
  var idx = 0
  var oldIdx = idx
  while idx < s.len:
    oldIdx = idx
    fastRuneAt(s, idx, rune)
    if rune.toUtf8() == "⁻":
      s.delete(oldIdx, idx - 1)
      return true

proc parseExponent(s: var string, negative: bool): int =
  var buf: string
  let idxStart = s.parseUntil(digits)
  var idx = idxStart
  if idx > 0:
    let numDigits = s[idx .. ^1].runeLen
    var seen = 0
    while idx < s.len:
      var buf: Rune
      fastRuneAt(s, idx, buf)
      inc seen
      let val = digits.find(buf.toUtf8()) * 10^(numDigits - seen)
      result += val
    result = if negative: -result else: result
    # remove `idx` onwards from s
    s.delete(idxStart, s.len)
  else:
    # no exponent means `1`
    result = 1

proc isBaseUnit(s: string): bool =
  result = s in ["g", "Gram", "m", "Meter", "s", "Second",
                 "A", "Ampere", "K", "Kelvin", "mol", "Mol",
                 "cd", "Candela"]
proc parseUnitKind(s: string): UnitKind =
  ## `s` must not contain anything aside from a CT unit!
  ## NOTE: we do not use `parseEnum` due to the shorthand notations for units
  case s
  of "g", "Gram": result = ukGram
  #of "kg": result = ukKiloGram
  of "m", "Meter": result = ukMeter
  of "s", "Second": result = ukSecond
  of "A", "Ampere": result = ukAmpere
  of "K", "Kelvin": result = ukKelvin
  of "mol", "Mol": result = ukMol
  of "cd", "Candela": result = ukCandela
  # derived SI units
  of "N", "Newton": result = ukNewton
  of "J", "Joule": result = ukJoule
  of "V", "Volt": result = ukVolt
  of "Hz", "Hertz": result = ukHertz
  of "C", "Coulomb": result = ukCoulomb
  of "W", "Watt": result = ukWatt
  of "Ω", "Ohm": result = ukOhm
  of "H", "Henry": result = ukHenry
  of "F", "Farad": result = ukFarad
  of "Pa", "Pascal": result = ukPascal
  of "bar", "Bar": result = ukBar
  # natural units
  of "NaturalLength": result = ukNaturalLength # length:
  of "NaturalMass": result = ukNaturalMass # mass:
  of "NaturalTime": result = ukNaturalTime# time:
  of "NaturalEnergy": result = ukNaturalEnergy# energy:
  # additional units
  of "eV", "ElectronVolt": result = ukElectronVolt
  of "lbs", "Pound": result = ukPound # lbs (lb singular is too uncommon):
  of "inch", "Inch": result = ukInch # in ( or possibly "inch" due to in being keyword):
  of "mi", "Mile": result = ukMile
  else: result = ukUnitLess

proc getUnitTypeImpl(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkSym:
    if n.strVal.isBaseUnit: return n
    if n.strVal == "float": return n
    let nTyp = n.getImpl
    result = nTyp.getUnitTypeImpl
  of nnkBracketExpr:
    if n[0].strVal.normalize == "distinct":
      result = n[1]
    elif n[0].strVal.normalize == "typedesc":
      result = n[1]
    else:
      error("Unexpected node: " & n.treerepr & " in `getUnitType`!")
  of nnkTypeDef:
    case n[2].kind
    of nnkDistinctTy:
      result = n[0] # in case child is distinct use parent [2][0] #
    of nnkSym: result = getUnitTypeImpl(n[2])
    else: error("Unexpected node: " & n.treerepr & " in `getUnitType`!")
  else:
    error("Unexpected node: " & n.treerepr & " in `getUnitType`!")

proc getUnitType(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  else: result = n.getTypeInst.getUnitTypeImpl()

proc isUnitLessNumber(n: NimNode): bool =
  case n.kind
  of nnkIntLit .. nnkFloatLit: result = true
  of nnkIdent: result = false # most likely direct unit
  else:
    let nTyp = n.getTypeInst
    if nTyp.kind == nnkSym:
      ## TODO: improve this check / include other numeric types
      if nTyp.strVal in ["float", "float64", "int", "int64"]:
        result = true

proc parseCTUnit(x: NimNode): CTCompoundUnit =
  ## 1. split by `•`
  ## for el in splits
  ##   2. parse possible si prefix
  ##   2a. remove prefix from string
  ##   3. parse possible negative unicode char
  ##   3a. parse possible exponent
  ##   4. parse name of unit
  ## Complication: We have 3 different notations for units
  ## a) m, kg, m•s⁻², ...
  ## b) Meter, Kg, Meter•Second⁻², ...
  ## c) "meter per second squared" -> "meterPerSecondSquared"
  ## a) and b) can be parsed together by both looking for `m` as well as `Meter` in each
  ## element. Verbose always start capital letters, shorthand depending on SI prefix / unit (N, V, A...)
  echo x.treerepr
  if x.isUnitLessNumber:
    echo "is unitless ", x.treerepr
    return # unit less number
  let x = x.getUnitType
  var xT = x.strVal
  let xTStrs = xT.split("•")
  for el in xTStrs:
    var mel = el
    let prefix = mel.parseSiPrefix
    let negative = hasNegativeExp(mel)
    let exp = parseExponent(mel, negative)
    echo "PARSING ", mel
    let unitKind = parseUnitKind(mel)
    echo unitKind
    ## hacky way to detect if this unit is written in short hand `m` vs. verbose `Meter`
    let isShortHand = if parseEnum[UnitKind](mel, ukUnitLess) == ukUnitLess and mel != "UnitLess": true else: false
    let ctUnit = initCTUnit(el, unitKind, exp, prefix,
                            isShortHand = isShortHand)
    result.add ctUnit

proc toBaseTypeScale(u: CTUnit): float =
  result = u.siPrefix.toFactor()
  result *= u.factor
  if u.unitKind == ukGram:
    result /= 1e3 # base unit is `kg`!
  result = pow(result, u.power.float)

proc toBaseTypeScale(x: CTCompoundUnit): float =
  ## returns the scale required to turn `x` to its base type, i.e.
  ## turn all units that are not already to SI form
  result = 1.0
  echo x.repr
  for u in x.units:
    result *= toBaseTypeScale(u)

proc toBaseType(u: CTUnit): CTUnit =
  result = u
  case u.unitKind
  of ukGram:
    ## SI unit base of Gram is KiloGram
    result.siPrefix = siKilo
    result.b.baseUnit = buGram
    result.b.siPrefix = siKilo
  else: result.siPrefix = siIdentity

proc toBaseType(x: CTCompoundUnit): CTCompoundUnit =
  ## converts `x` to a unit representing the base type.
  ## WARNING: this is a lossy conversion, so make sure to extract the
  ## conversion scales using `toBaseTypeScale` before doing this!
  ## TODO: can we add to `CTUnit` a scale?
  for u in x.units:
    result.add u.toBaseType

macro `+`*(x, y: typed): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  if xCT == yCT or xCT.isUnitLess or yCT.isUnitLess:
    # excactly the same type, just add
    # TODO: deduce the "nicest" type representation
    let resType = if xCT.isUnitLess: y.getTypeInst else: x.getTypeInst
    result = quote do:
      defUnit(`resType`)
      (`x`.float + `y`.float).`resType`
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    xCT = xCT.toBaseType().simplify()
    yCT = yCT.toBaseType().simplify()
    ## TODO: add an `equivalent` procedure maybe? Currently due to Compound and seq of
    ## non compound units we may not end up at the same exact type here!
    ## now xCT and yCT have to be the same
    # doAssert xCT == yCT, "Conversion to base types failed!"
    let resType = xCT.toNimType()
    result = quote do:
      defUnit(`resType`)
      (`x`.float * `xScale` + `y`.float * `yScale`).`resType`
  else:
    error("Different quantities cannot be added! Quantity 1: " & $x.repr & ", Quantity 2: " & $y.repr)

macro `-`*(x, y: typed): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  if xCT == yCT or xCT.isUnitLess or yCT.isUnitLess:
    # excactly the same type, just add
    # TODO: deduce the "nicest" type representation
    let resType = if xCT.isUnitLess: y.getTypeInst else: x.getTypeInst
    result = quote do:
      defUnit(`resType`)
      (`x`.float - `y`.float).`resType`
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    xCT = xCT.toBaseType().simplify()
    yCT = yCT.toBaseType().simplify()
    ## TODO: add an `equivalent` procedure maybe? Currently due to Compound and seq of
    ## non compound units we may not end up at the same exact type here!
    ## now xCT and yCT have to be the same
    # doAssert xCT == yCT, "Conversion to base types failed!"
    let resType = xCT.toNimType()
    result = quote do:
      defUnit(`resType`)
      (`x`.float * `xScale` - `y`.float * `yScale`).`resType`
  else:
    error("Different quantities cannot be subtracted! Quantity 1: " & $x.repr & ", Quantity 2: " & $y.repr)
proc convertIfMultipleSiPrefixes(x: CTCompoundUnit): CTCompoundUnit =
  ## checks if any CTUnit appears multiple times with a different SI prefixes
  var unitTab = initTable[UnitKind, SiPrefix]()
  var convertSet: set[UnitKind]
  for u in x.units:
    if u.unitKind in unitTab and unitTab[u.unitKind] != u.siPrefix:
      convertSet.incl u.unitKind
    else:
      unitTab[u.unitKind] = u.siPrefix

  for u in x.units:
    if u.unitKind in convertSet:
      echo "INFO: Auto converting units of ", $u, " to base unit to simplify"
      let scale = u.toBaseTypeScale()
      var uBase = u.toBaseType()
      uBase.factor *= scale
      result.add uBase
    else:
      result.add u

macro `*`*(x, y: typed): untyped =
  var xCT = parseCTUnit(x)
  let yCT = parseCTUnit(y)
  # add `yCT` to xCT. Equates a product after simplification
  xCT.add yCT
  # TODO: automatically perform scaling to SI units?
  var resTypeCT = xCT.flatten().convertIfMultipleSiPrefixes()
  let scale = resTypeCT.toBaseTypeScale()
  ## TODO: check if there are multiple SI prefixes of the same units present.
  ## If so, convert to base units, else do not.
  let resType = resTypeCT.simplify().toNimType()
  echo xCT.repr
  echo resType.treerepr
  result = quote do:
    defUnit(`resType`)
    (`x`.float * `y`.float * `scale`).`resType`

macro `/`*(x, y: typed): untyped =
  var xCT = parseCTUnit(x)
  let yCT = parseCTUnit(y)
  # add inverted `yCT` (power -> -power) to xCT. Equates a division after simplification
  xCT.add yCT.invert()
  ## TODO: automatically perform scaling to SI units?
  var resTypeCT = xCT.flatten().convertIfMultipleSiPrefixes()
  let scale = resTypeCT.toBaseTypeScale()
  let resType = resTypeCT.simplify().toNimType()
  result = quote do:
    defUnit(`resType`)
    (`x`.float / `y`.float * `scale`).`resType`

proc commonQuantity(x: typedesc, y: typedesc): bool =
  ## checks if x and y share a common
  ## NOTE: duplicate of above due to typedesc annoyances....
  let xCT = x.getTypeInst.parseCTUnit()
  let yCT = y.getTypeInst.parseCTUnit()
  result = xCT.commonQuantity(yCT)

macro determineScale(x: typedesc, y: typedesc): float =
  ## x and y `have` to be of the same quantity
  # determine which SI prefix ``in total``
  # for that:
  # - get string representation of type
  # - need to split by `•`
  # - walk string and check for SI prefix
  # - if prefix found, set `factor`
  # - if exponent found `⁻` take `factor=factor^power`
  # - multiply `scale` by `factor`
  let xScale = x.parseCTUnit().toBaseTypeScale()
  let yScale = y.parseCTUnit().toBaseTypeScale()
  echo "X ", xScale
  echo "Y ", yScale
  result = newLit(xScale / yScale)

proc to*[T; U](x: T, to: typedesc[U]): U =
  ## TODO: replace by macro as well so that we can deal with arbitrary types
  ##
  ## check if conversion possible

  ## TODO: check here if we convert between equivalent quantities, but different
  ## units (`not` just difference in SI). Or rather extend `determineScale`.
  when T is U:
    result = x
  elif commonQuantity(T, U):
    # perform conversion
    ## thus determine scaling factor due to different SI prefixes
    let scale = determineScale(T, U)
    result = (x.float * scale).U
  else:
    {.error: "Cannot convert " & $T & " to " & $U & " as they represent different " &
      "quantities!".}

macro toImpl(x: typed, to: static CTCompoundUnit): NimNode =
  ## TODO: replace by macro as well so that we can deal with arbitrary types
  ##
  ## check if conversion possible
  let xCT = parseCTUnit(x)
  let yCT = to #parseCTUnit(to)
  if xCT == yCT:
    result = x
  elif xCT.commonQuantity(yCT):
    # perform conversion
    ## thus determine scaling factor due to different SI prefixes
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    let scale = xScale / yScale
    let resType = yCT.toNimType()
    result = quote do:
      when not declared(`resType`):
        type `resType` = distinct CompoundQuantity
      (`x`.float * `scale`).`resType`
  else:
    error("Cannot convert " & $(xCT.toNimType()) & " to " & $(yCT.toNimType()) & " as they represent different " &
      "quantities!")

#macro to(x: untyped, to: untyped): untyped =
#  let yCT = parseCTUnit(to)
#  result = quote do:
#    toImpl(`x`, `yCT`)

{.experimental: "dotOperators".}
macro `.`*(x: typed, y: untyped): untyped =
  ## macro to allow to generate new types on the fly
  ## TODO: still need a way to map e.g. `N` to newton from a string
  let typX = x.getTypeInst()
  let yCT = y.parseCTUnit()
  let resType = yCT.simplify().toNimType()
  result = quote do:
    when not declared(`resType`):
      type `resType` = distinct CompoundQuantity
    (`x`.float).`resType`

let
  c* = 299792458.0.Meter•Second⁻¹
  ε_0* = 8.8541878128e-12.A•s•V⁻¹•m⁻¹
  e* = 1.602176634e-19.C
  m_e* = 9.1093837015e-31.kg
  m_e_c2* = 0.510998928.MeV # MeV
  N_A* = 6.02214076e23.mol⁻¹
  M_u* = 0.99999999965e-3.kg•mol⁻¹
  m_μ* = 1.883531627e-27.kg # 105.6583755e3 # MeV / c² # 1.883531627e-27 kg
  m_μ_eV* = 105.6583755e6.eV # / c²
  π* = PI
  r_e* = e*e / (4 * π * ε_0 * m_e * c * c) # classical electron radius
  #K = 4 * π * N_A * r_e * r_e * m_e_c2 * (100.0^2)# [MeV mol⁻¹ cm²]
