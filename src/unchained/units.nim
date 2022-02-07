import math, macros, options, sequtils, algorithm, sets, tables, strutils, unicode, typetraits, strformat, parseutils

when false:
  type
    UnitObject = object
      quantity: foo
      name: foo

#[
Extend the notion of CTBaseUnit to some general unit object. These unit objects we can
store in a CT table, mapping their unit names to the objects. That way we
1. don't have to parse into `CTUnit` every time again
2. we should be able to more easily work towards ??? lost my train of thought.
]#


type
  Unit* = distinct float

  Quantity* = distinct Unit
  CompoundQuantity* = distinct Quantity

  UnitLess* = distinct Unit

  ## Base Quantities
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
  Area* = distinct CompoundQuantity

  ElectricPotential* = distinct CompoundQuantity
  Voltage* = ElectricPotential

  Frequency* = distinct CompoundQuantity

  Charge* = distinct CompoundQuantity
  Power* = distinct CompoundQuantity
  ElectricResistance* = distinct CompoundQuantity
  Capacitance* = distinct CompoundQuantity
  Inductance* = distinct CompoundQuantity
  Pressure* = distinct CompoundQuantity
  MagneticFieldStrength* = distinct CompoundQuantity

  # angles and solid angles are technically UnitLess.
  Angle* = UnitLess
  SolidAngle* = UnitLess

  DerivedQuantity* = Velocity | Acceleration | Momentum | Force | Energy | Density | ElectricPotential | Voltage |
    Frequency | Charge | Power | ElectricResistance | Capacitance | Inductance | Pressure | Angle | SolidAngle |
    MagneticFieldStrength

  SomeQuantity* = BaseQuantity | DerivedQuantity

import macrocache

## Knows about *all* units. Good to check if something (or a part of something) is a unit at all
const PredefinedUnitImpls = CacheTable"PredefinedUnitImpls" # the implementations
const PredefinedUnits = CacheTable"PredefinedUnits" # the actual unit symbols
const GeneratedUnits = CacheTable"GeneratedUnits"
## Add other CT tables...
# const

proc contains*(t: CacheTable, key: string): bool =
  for k, val in pairs(t):
    if k == key: return true

proc isPredefined(n: NimNode): bool =
  doAssert n.kind in {nnkSym, nnkIdent}
  result = n.strVal in PredefinedUnits

proc exportType(n: NimNode): NimNode = nnkPostfix.newTree(ident"*", n)

macro defineUnits(stmts: untyped): untyped =
  ##
  result = nnkTypeSection.newTree()
  for stmt in stmts:
    case stmt.kind
    of nnkCommentStmt: result.add stmt
    of nnkAsgn:
      let asTyp = stmt[0]
      let isTyp = stmt[1]
      PredefinedUnitImpls[asTyp.strVal] = isTyp
      PredefinedUnits[asTyp.strVal] = asTyp
      result.add nnkTypeDef.newTree(exportType(asTyp), newEmptyNode(), isTyp)
    else: error("invalid " & $stmt.kind)
  #echo result.treerepr

defineUnits:
  #Joule* = distinct Unit
  ## Base SI units
  Second = distinct Time
  Meter = distinct Length
  Gram = distinct Mass
  KiloGram = distinct Gram ## KiloGram is special due to being the actual SI unit. Thus defined here
  Ampere = distinct Current
  Kelvin = distinct Temperature
  Mol = distinct AmountOfSubstance
  Candela = distinct Luminosity

  SiUnit = Second | Meter | KiloGram | Ampere | Kelvin | Mol | Candela

  ## compound units, i.e. definition of different physical concepts.
  KiloGram•Meter•Second⁻¹ = distinct Momentum
  Second² = distinct Time
  Meter•Second⁻¹ = distinct Velocity
  Meter•Second⁻² = distinct Acceleration
  KiloGram•Meter²•Second⁻² = distinct Energy
  KiloGram•Meter•Second⁻² = distinct Force
  Second⁻¹ = distinct Frequency
  KiloGram•Meter²•Second⁻³ = distinct Power
  Ampere•Second = distinct Charge
  KiloGram•Meter²•Second⁻²•Ampere⁻² = distinct Inductance
  Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹ = distinct Capacitance
  KiloGram•Meter²•Ampere⁻¹•Second⁻³ = distinct ElectricPotential
  KiloGram•Meter²•Second⁻³•Ampere⁻² = distinct ElectricResistance
  KiloGram•Meter⁻¹•Second⁻² = distinct Pressure
  KiloGram•Meter⁻³ = distinct Density
  KiloGram•Ampere⁻¹•Second⁻² = distinct MagneticFieldStrength
  # the following two are a bit problematic, as this is not a rea
  # identity. `Meter•Meter⁻¹` can, but does not ``need`` to be an angle.
  Meter•Meter⁻¹ = distinct Angle
  Meter²•Meter⁻² = distinct SolidAngle


  ## derived SI units
  ## TODO: are these actually needed? We do all work in the internal CT
  ## unit base anyway.
  ## Well, but these help if the user computes such combinations manually
  ## and gets those units.
  Newton = KiloGram•Meter•Second⁻²
  Joule = KiloGram•Meter²•Second⁻²
  Volt = KiloGram•Meter²•Ampere⁻¹•Second⁻³
  Hertz = Second⁻¹
  Coulomb = Ampere•Second
  Watt = KiloGram•Meter²•Second⁻³ # Joule•Second⁻¹
  Ohm = KiloGram•Meter²•Second⁻³•Ampere⁻²
  Henry = KiloGram•Meter²•Second⁻²•Ampere⁻²
  Farad = Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹
  Pascal = KiloGram•Meter⁻¹•Second⁻²
  ## TODO: distinct quantity of Magnetic
  Tesla = KiloGram•Ampere⁻¹•Second⁻²
  # radian and steradian are distinct versions as they should not be converted
  # to that representation. Also Meter•Meter⁻¹ is not necessarily an angle.
  # TODO: think about removing Meter•Meter⁻¹ from here and only writing as
  # `distinct Angle`.
  Radian = distinct Meter•Meter⁻¹
  Steradian = distinct Meter²•Meter⁻²
  Becquerel = distinct Second⁻¹

  ## other units
  ElectronVolt = distinct Energy
  Bar = distinct Pressure
  Liter = distinct Length # TODO make that Volume
  Degree = distinct Angle
  Minute = distinct Time
  Hour = distinct Time
  Day = distinct Time
  Year = distinct Time

  ## Imperial
  Pound = distinct Mass
  Inch = distinct Length
  Mile = distinct Length
  Foot = distinct Length
  Yard = distinct Length
  Acre = distinct Area
  Ounce = distinct Mass
  Slug = distinct Mass
  PoundForce = distinct Force

  ## possibly define convenient overloads? Not really required, since we compute that these match after
  ## all, no? E.g. given Joule•Coulomb⁻¹. We would parse each, convert to base SI units and notice that
  ## it's the same as required `Volt` after conversion to base SI units for V. That's how it should work
  ## anyway.
  Joule•Coulomb⁻¹ = Volt
  Ampere•Ohm = Volt

  DerivedSiUnits = Newton | Joule

  ## shorthand types
  m = Meter
  s = Second
  A = Ampere
  g = Gram
  Kg = KiloGram
  kg = Kg ## XXX: This should go, but it's a good case to think about how to resolve aliases!
  K = Kelvin
  mol = Mol
  cd = Candela
  N = Newton
  J = Joule
  V = Volt
  Hz = Hertz
  C = Coulomb
  W = Watt
  Ω = Ohm
  H = Henry
  F = Farad
  Pa = Pascal
  bar = Bar
  rad = Radian
  sr = Steradian
  eV = ElectronVolt
  ° = Degree
  min = Minute
  h = Hour
  day = Day
  yr = Year
  L = Liter
  T = Tesla
  Bq = Becquerel

  ## imperial shorthand
  inch = Inch
  mi = Mile
  lbs = Pound
  ft = Foot
  yd = Yard
  acre = Acre
  oz = Ounce
  slug = Slug
  lbf = PoundForce

  # common compound units
  m•s⁻² = Meter•Second⁻²
  ## TODO: this should just be the long form, no?
  g•cm⁻³ = distinct Density
  # english language versios
  meterPerSecondSquared = Meter•Second⁻²

type
  SiPrefix* = enum
    siYocto, siZepto, siAtto, siFemto, siPico, siNano, siMicro, siMilli, siCenti, siDeci,
    siIdentity,
    siDeca, siHecto, siKilo, siMega, siGiga, siTera, siPeta, siExa, siZetta, siYotta

  QuantityKind* = enum
    # base quantities
    qkUnitLess, qkMass, qkLength, qkTime, qkCurrent, qkTemperature, qkAmountOfSubstance, qkLuminosity,
    # derived quantities
    qkFrequency, qkVelocity, qkAcceleration, qkArea, qkMomentum, qkForce, qkEnergy, qkElectricPotential,
    qkCharge, qkPower, qkElectricResistance, qkInductance, qkCapacitance, qkPressure, qkDensity,
    qkAngle, qkSolidAngle, qkMagneticFieldStrength, qkActivity

  ## enum storing all known units (their base form) to allow easier handling of unit conversions
  ## Enum value is the default name of the unit. Note: Order is important! (e.g. for `isCompound`)
  UnitKind* = enum
    ukUnitLess = "UnitLess"
    ukGram = "Gram"
    ukMeter = "Meter"
    ukSecond = "Second"
    ukAmpere = "Ampere"
    ukKelvin = "Kelvin"
    ukMol = "Mol"
    ukCandela = "Candela"
    # derived SI units, all compound
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
    ukRadian = "Radian"
    ukDegree = "Degree"
    ukSteradian = "Steradian"
    ukTesla = "Tesla"
    ukBecquerel = "Becquerel"
    # natural units
    ukNaturalLength = "NaturalLength" # length
    ukNaturalMass = "NaturalMass" # mass
    ukNaturalTime = "NaturalTime" # time
    ukNaturalEnergy = "NaturalEnergy" # energy
    # ...
    # additional compound units
    ukElectronVolt = "ElectronVolt"
    ukLiter = "Liter"
    # additional non compound units
    ukMinute = "Minute"
    ukHour = "Hour"
    ukDay = "Day"
    ukYear = "Year"
    # imperial units
    ukPound = "Pound" # lbs (lb singular is too uncommon)
    ukInch = "Inch" # in ( or possibly "inch" due to in being keyword)
    ukMile = "Mile"
    ukFoot = "Foot"
    ukYard = "Yard"
    ukOunce = "Ounce"
    ukSlug = "Slug"
    ukAcre = "Acre"
    ukPoundForce = "Pound-force"
    # ...

  ## Base unit kind stores the fundamental units, which represent the SI units (except for Gram in place of kg)
  ## that each represent one of the fundamental quantities (the "base quantities" in `QuantityKind`).
  ## NOTE: The order of `BaseUnitKind`, `UnitKind` and `QuantityKind` is important, both for sorting
  ## units as well as to allow conversion between the base units / quantities using `ord`.
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
    siPrefix: float # as a pure float value

  ## `CTCompoundQuantity` is a helper that is used to determine if a unit is `equivalent` to one another.
  ## Equivalence of units means that the actual `dimensions` (in terms of real base quantities:
  ## time, length, mass, current, temperature, amount of substance, luminosity) are the same.
  ## For this it does not matter if we compare `lbs` and `kg` or `eV` and `J`. Only the final
  ## powers of the base quantities matters. The base quantities can be interpreted as a system of
  ## basis vectors and equivalence implies two vectors are the same up to some scaling, i.e. they
  ## are linearly dependent.
  CTCompoundQuantity = Table[QuantityKind, int]

# parsing CT units is the basis of all functionality almost
proc parseCTUnit(x: NimNode): CTCompoundUnit
proc toNimType(x: CTCompoundUnit): NimNode
proc toNimType(u: CTUnit): string
proc flatten(units: CTCompoundUnit): CTCompoundUnit
proc simplify(x: CTCompoundUnit): CTCompoundUnit
proc toBaseType(x: CTCompoundUnit): CTCompoundUnit
proc toFactor(prefix: SiPrefix): float

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
  result.add newLit"Unit"
  result.add newLit"Quantity"
  result.add newLit"SiUnit"
  result.add newLit"DerivedSiUnits"
  result.add newLit"SomeQuantity"
  result.add newLit"DerivedQuantity"
  result.add newLit"BaseQuantity"

const qTypes* = quantityList()

proc resolveAlias(n: NimNode): NimNode =
  ## returns the first type that is `distinct` (i.e. convert Newton -> KiloGram•Meter•Second⁻²)
  case n.kind
  of nnkDistinctTy: result = n
  of nnkBracketExpr:
    if n[1].kind == nnkSym:
      result = n[1].getImpl.resolveAlias
    else:
      result = n[1].resolveAlias
  of nnkSym:
    if n.getTypeInst.kind != nnkSym: result = n.getTypeInst.resolveAlias
    elif n.getTypeImpl.kind != nnkSym: result = n.getTypeImpl.resolveAlias
    elif n.getImpl.kind != nnkSym: result = n.getImpl.resolveAlias
    else: result = n
  of nnkTypeDef:
    case n[2].kind
    of nnkDistinctTy: result = n[0]
    of nnkInfix: result = n[0]
    of nnkObjectTy: result = newEmptyNode()
    of nnkRefTy: result = newEmptyNode()
    of nnkPtrTy: result = newEmptyNode()
    else: result = n[2].getImpl.resolveAlias
  else: result = newEmptyNode()

macro isAUnit*(x: typed): untyped =
  ## NOTE: it's really hard to replace this by something cleaner :/
  ## Ideally this should be replaced by something that uses shared logic with
  ## `getUnitTypeImpl` & making use of CT tables (possibly of objects?)
  let x = x.resolveAlias()
  case x.kind
  of nnkSym, nnkDistinctTy:
    let typ = x
    var xT = if typ.kind == nnkDistinctTy: typ[0] else: typ
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

proc resolveTypeFromAlias(n: NimNode): NimNode =
  case n.kind
  of nnkSym:
    let typ = n.getImpl
    doAssert typ.kind == nnkTypeDef, " no, was " & $typ.treerepr
    if typ[2].typeKind == ntyAlias:
      result = typ[2].resolveTypeFromAlias()
    else:
      case typ[2].kind
      of nnkInfix: # this is a type class A = B | C | D, ... return input
        result = n
      else:
        result = typ[2]
  else:
    let typ = n.getTypeImpl
    doAssert typ.kind == nnkDistinctTy, " no, was " & $typ.treerepr
    result = typ[0]

proc resolveTypeFromDistinct(n: NimNode): NimNode =
  let typ = n.getImpl
  doAssert typ.kind == nnkTypeDef
  result = typ[0]

proc resolveTypeFromTypeDesc(n: NimNode): NimNode =
  let typ = n.getType
  doAssert typ.kind == nnkBracketExpr, "no, was " & $typ.treerepr
  result = typ[1]

proc getUnitTypeImpl(n: NimNode): NimNode =
  case n.typeKind
  of ntyAlias: result = n.resolveTypeFromAlias()
  of ntyDistinct: result = n.resolveTypeFromDistinct()
  of ntyTypeDesc: result = n.resolveTypeFromTypeDesc()
  else: error("Unsupported : " & $n.typeKind)

proc pretty*[T: SomeUnit](s: T, precision: int): string =
  result = s.float.formatFloat(precision = precision)
  result.trimZeros()
  result.add &" {$typeof(s)}"

proc `$`*[T: SomeUnit](s: T): string = pretty(s, precision = -1)

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
const DigitsAscii = ["0","1","2","3","4","5","6","7","8","9"]
const AsciiChars = {'*', '^', '-', '0' .. '9'}

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
      if typStr.strVal == "KiloGram": continue # skip generation of `KiloGram`
      let isTyp = nnkDistinctTy.newTree(typ)
      result.add nnkTypeDef.newTree(nnkPostfix.newTree(ident"*", typStr), newEmptyNode(), isTyp)
  if genShort:
    for (si, prefix) in SiPrefixStringsShort:
      if prefix == siIdentity: continue
      if si in excludes:
        result.add ident("_") # if we exclude, add placeholder. Used to mark for cross reference long / short
        continue
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
  (rad, Radian)
  (sr, Steradian)
  (T, Tesla) exclude [f]
  (Bq, Becquerel)

proc isUnitLess(u: CTCompoundUnit): bool = u.units.len == 0

proc isCompound(unitKind: UnitKind): bool =
  result = unitKind notin {ukUnitLess .. ukCandela,
                           ukNaturalLength .. ukNaturalTime,
                           ukMinute .. ukSlug}

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
  of ukRadian: result = qkAngle
  of ukSteradian: result = qkSolidAngle
  of ukTesla: result = qkMagneticFieldStrength
  of ukBecquerel: result = qkActivity
  # natural units
  of ukNaturalLength: result = qkLength
  of ukNaturalMass: result = qkMass
  of ukNaturalTime: result = qkTime
  of ukNaturalEnergy: result = qkEnergy
  # other units
  of ukElectronVolt: result = qkEnergy
  of ukDegree: result = qkAngle
  of ukMinute: result = qkTime
  of ukHour: result = qkTime
  of ukDay: result = qkTime
  of ukYear: result = qkTime
  of ukLiter: result = qkLength
  of ukPound: result = qkMass
  of ukInch: result = qkLength
  of ukMile: result = qkLength
  of ukFoot: result = qkLength
  of ukYard: result = qkLength
  of ukOunce: result = qkMass
  of ukSlug: result = qkMass
  of ukAcre: result = qkArea
  of ukPoundForce: result = qkForce

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
  of ukMinute: result = buSecond
  of ukHour: result = buSecond
  of ukDay: result = buSecond
  of ukYear: result = buSecond
  of ukPound: result = buGram
  of ukInch: result = buMeter
  of ukMile: result = buMeter
  of ukFoot: result = buMeter
  of ukYard: result = buMeter
  of ukOunce: result = buGram
  of ukSlug: result = buGram
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

proc toQuantity(baseUnit: BaseUnitKind): QuantityKind =
  ## Convert a given base unit to its quantity
  ## BaseUnitKind and QuantityKind `have` to have the same order!
  result = QuantityKind(ord(baseUnit))

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
  of ukRadian:
    result.add toCTBaseUnit(ukMeter, power = 1)
    result.add toCTBaseUnit(ukMeter, power = -1)
  of ukDegree:
    result.add toCTBaseUnitSeq(ukRadian)
  of ukSteradian:
    result.add toCTBaseUnit(ukMeter, power = 2)
    result.add toCTBaseUnit(ukMeter, power = -2)
  of ukTesla:
    result.add toCTBaseUnit(ukGram, siPrefix = siKilo)
    result.add toCTBaseUnit(ukAmpere, power = -1)
    result.add toCTBaseUnit(ukSecond, power = -2)
  of ukBecquerel:
    result.add toCTBaseUnit(ukSecond, power = -1)
  of ukElectronVolt:
    ## our logic is incomplete, since it's missing proper conversions ouside of SI prefixes!
    result.add toCTBaseUnitSeq(ukJoule)
  of ukLiter:
    result.add toCTBaseUnit(ukMeter, power = 3)
  # natural units
  of ukNaturalEnergy:
    result.add toCTBaseUnitSeq(ukJoule)
  of ukAcre:
    result.add toCtBaseUnit(ukMeter, power = 2)
  of ukPoundForce:
    result.add toCtBaseUnitSeq(ukNewton)
  else: error("Non compound unit `" & $unitKind & "` cannot be converted to sequence of CTBaseUnit!")

proc getConversionFactor(unitKind: UnitKind): float =
  case unitKind
  of ukElectronVolt: result = 1.602176634e-19 # relative to: Joule
  of ukPound: result = 0.45359237 # relative to: kg
  of ukMile: result = 1600 # relative to: m
  of ukInch: result = 0.0254 # relative to: cm
  of ukBar: result = 100_000 # relative to Pa
  of ukDegree: result = PI / 180.0 # relative to Radian
  of ukMinute: result = 60.0
  of ukHour: result = 3600.0
  of ukDay: result = 86400.0
  of ukYear: result = 365.0 * 86400.0
  of ukLiter: result = 1e-3 # relative to: m³
  of ukFoot: result = 0.3048 # relative to m
  of ukYard: result = 0.9144 # relative to m
  of ukOunce: result = 28.349523125 * 1e-3 # relative to kg
  of ukSlug: result = 14.59390294 # relative to kg
  of ukAcre: result = 4046.8564224 # relative to m²
  of ukPoundForce: result = 4.44822162 # relative to N
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
                isShortHand = false, factor = none(float)): CTUnit =
  result = unitKind.toCTUnit()
  result.name = name
  if factor.isSome:
    result.factor = factor.get
  result.power = power
  result.siPrefix = siPrefix #if siPrefix == siIdentity and unitKind == ukGram: siKilo else: siPrefix
  result.isShortHand = isShortHand

#proc resolveToBaseType(n: NimNode):

## This is a *forced* conversion of a unit to a float. It simply removes any unit from
## the type. Currently just `x.float`. Might change in the future.
proc toFloat*[T: SomeUnit](x: T): float = x.float

## auto conversion of `UnitLess` to `float` is possible so that e.g. `sin(5.kg / 10.kg)` works as expected!
converter toRawFloat*(x: UnitLess): float = x.float
converter toUnitLess*(x: SomeNumber): UnitLess = x.UnitLess
converter toUnitLess*(x: float64): UnitLess = x.UnitLess
converter toRawFloat*(x: Radian): float = x.float
converter toRawFloat*(x: Steradian): float = x.float
converter toRadian*(x: float): Radian = x.Radian
converter toSteradian*(x: float): Steradian = x.Steradian

import hashes
proc hash*[T: SomeUnit](x: T): Hash =
  result = result !& hash($typeof(T))
  result = result !& hash(x.float)
  result = !$result

macro defUnit*(arg: untyped): untyped =
  ## Helper template to define new units (not required to be used manually)
  let argCT = parseCTUnit(arg)
  let shortHand = argCT.units.allIt(it.isShortHand)

  ## TODO: instead of just using the long version, what to do for
  ## J•m or something like this? For max compatibility the RHS
  ## should actually be the base unit stuff.
  if shortHand:
    ## note: flattening the given unit does *not* flatten units that have a conversion
    ## factor relative to the SI base unit, e.g. eV -> J, lbs -> kg etc. Thus flattening
    ## and simplifying yields, which is safely equivalent.
    let resType = argCT.flatten.simplify.toNimType()
    if resType.strVal != "UnitLess":
      result = quote do:
        when not declared(`resType`):
          type `resType` = distinct CompoundQuantity
        when not declared(`arg`):
          type `arg` = `resType`
    else:
      result = quote do:
        when not declared(`arg`):
          type `arg` = `resType`
  else:
    if arg.strVal != "UnitLess":
      result = quote do:
        when not declared(`arg`):
          type `arg` = distinct CompoundQuantity

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
  result.siPrefix = x.siPrefix

proc invert(x: CTCompoundUnit): CTCompoundUnit =
  for u in x.units:
    var unit = u
    case u.unitType
    of utQuantity:
      unit.power = -unit.power
      # base units' powers ``not`` inverted!
      result.add unit
    of utCompoundQuantity:
      unit.power = -unit.power
      # base units' powers ``not`` inverted!
      result.add unit

proc `==`(a, b: CTUnit): bool =
  result = (a.unitKind == b.unitKind and a.power == b.power and a.siPrefix == b.siPrefix)

proc `<`(a, b: CTUnit): bool =
  if a.unitKind < b.unitKind:
    result = true
  elif a.unitKind > b.unitKind:
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
      result = a.siPrefix < b.siPrefix

proc flatten(units: CTCompoundUnit): CTCompoundUnit =
  ## extracts all base units from individual compound CTUnits and
  ## turns it into a single CTCompoundUnit of only base units. Finally
  ## simplifies the result.
  var prefix = 1.0
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
      case u.unitKind
      # for these units we do `not` want to flatten them!
      # TODO: most of these are not compound though?!
      of ukElectronVolt, ukPound, ukInch, ukMile, ukBar, ukSteradian, ukRadian, ukLiter,
         ukFoot, ukYard, ukSlug, ukOunce, ukAcre, ukPoundForce: result.add u
      else:
        let power = u.power
        prefix *= u.siPrefix.toFactor # note: as we're looking at compounds,
                                      # no need to worry about `Gram / KiloGram`
        for b in u.bs:
          var mb = b
          mb.power = mb.power * power
          # prefix handle ?
          result.add mb
  result.siPrefix = prefix # assign the prefix

proc toCTQuantity(a: CTCompoundUnit): CTCompoundQuantity =
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

proc commonQuantity(a, b: CTCompoundUnit): bool =
  ## Comparison is done by checking for the same base units and powers using
  ## `CTCompoundQuantity`.
  let aQuant = a.toCTQuantity()
  let bQuant = b.toCTQuantity()
  result = aQuant == bQuant

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
  if aFlat.siPrefix != bFlat.siPrefix:
    return false
  result = true

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
  if u.unitKind == ukUnitLess: return
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
    if u.unitKind == ukUnitLess: continue
    var str = toNimType(u)
    if idx < xSorted.high:
      str.add "•"
    name.add str
  result = if name.len == 0: ident("UnitLess") else: ident(name)

proc parseUntil(s: string, chars, errorOn: openArray[string]): int =
  ## parses until one of the runes in `chars` is found
  var idx = 0
  var rune: Rune
  var oldIdx = idx
  while idx < s.len:
    oldIdx = idx
    fastRuneAt(s, idx, rune)
    if rune.toUtf8 in chars:
      return oldIdx
    elif rune.toUtf8 in errorOn:
      error("Invalid rune in input string: " & $(rune.toUtf8()) & ". Did you type " &
        "a non superscript power by accident?")
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
    if prefix == siPeta and (s.startsWith("Pound") or s == "Pa"): return siIdentity
    if prefix == siExa and s.startsWith("ElectronVolt"): return siIdentity
    if prefix == siMilli and s.startsWith("mol"): return siIdentity
    if prefix == siMilli and s.startsWith("min"): return siIdentity
    if prefix == siMega and s.startsWith("Minute"): return siIdentity
    if prefix == siMega and s.startsWith("Mol"): return siIdentity
    if prefix == siYocto and s.startsWith("yr"): return siIdentity
    if prefix == siYotta and s.startsWith("Year"): return siIdentity
    if prefix == siYocto and s.startsWith("yd"): return siIdentity
    if prefix == siYotta and s.startsWith("Yard"): return siIdentity
    if prefix == siTera and (s == "T" or s.startsWith("Tesla")): return siIdentity
    if prefix == siFemto and s == "ft": return siIdentity
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
  let idxStart = s.parseUntil(digits, errorOn = DigitsAscii)
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
  of "rad", "Radian": result = ukRadian
  of "sr", "Steradian": result = ukSteradian
  of "T", "Tesla": result = ukTesla
  of "Bq", "Becquerel": result = ukBecquerel
  # natural units
  of "NaturalLength": result = ukNaturalLength # length:
  of "NaturalMass": result = ukNaturalMass # mass:
  of "NaturalTime": result = ukNaturalTime# time:
  of "NaturalEnergy": result = ukNaturalEnergy# energy:
  # additional units
  of "eV", "ElectronVolt": result = ukElectronVolt
  of "°", "Degree": result = ukDegree
  of "min", "Minute": result = ukMinute
  of "h", "Hour": result = ukHour
  of "day", "Day": result = ukDay
  of "yr", "Year": result = ukYear
  of "L", "Liter": result = ukLiter
  of "lbs", "Pound": result = ukPound # lbs (lb singular is too uncommon):
  of "inch", "Inch": result = ukInch # in ( or possibly "inch" due to in being keyword):
  of "mi", "Mile": result = ukMile
  of "ft", "Foot": result = ukFoot
  of "yd", "Yard": result = ukYard
  of "oz", "Ounce": result = ukOunce
  of "slug", "Slug": result = ukSlug
  of "acre", "Acre": result = ukAcre
  of "lbf", "PoundForce", "Pound-force": result = ukPoundForce
  else: result = ukUnitLess

proc getUnitType(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkAccQuoted:
    var s: string
    for el in n:
      s.add el.strVal
    result = ident(s)
  else: result = n.getTypeInst.getUnitTypeImpl()

proc isUnitLessNumber(n: NimNode): bool =
  case n.kind
  of nnkIntLit .. nnkFloatLit: result = true
  of nnkIdent: result = false # most likely direct unit
  of nnkAccQuoted:
    ## TODO: disallow things that are not a number?
    result = false
  else:
    let nTyp = n.getTypeInst
    if nTyp.kind == nnkSym:
      ## TODO: improve this check / include other numeric types
      if nTyp.strVal in ["float", "float64", "int", "int64"]:
        result = true

proc parseCTUnitUnicode(x: string): CTCompoundUnit =
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
  let xTStrs = x.split("•")
  for el in xTStrs:
    var mel = el
    let prefix = mel.parseSiPrefix
    let negative = hasNegativeExp(mel)
    let exp = parseExponent(mel, negative)
    let unitKind = parseUnitKind(mel)
    ## hacky way to detect if this unit is written in short hand `m` vs. verbose `Meter`
    let isShortHand = if parseEnum[UnitKind](mel, ukUnitLess) == ukUnitLess and mel != "UnitLess": true else: false
    let ctUnit = initCTUnit(el, unitKind, exp, prefix,
                            isShortHand = isShortHand)
    result.add ctUnit

proc parseCTUnitAscii(x: string): CTCompoundUnit =
  var idx = 0
  var buf: string
  while idx < x.len:
    idx += x.parseUntil(buf, until = '*', start = idx)
    let powIdx = buf.find("^")
    doAssert powIdx < buf.high, "Invalid unit, ends with `^`: " & $buf
    let exp = if powIdx > 0: parseInt(buf[powIdx + 1 .. buf.high])
              else: 1
    var mel = if powIdx > 0: buf[0 ..< powIdx]
              else: buf
    let prefix = mel.parseSiPrefix
    let unitKind = parseUnitKind(mel)
    let isShortHand = if parseEnum[UnitKind](mel, ukUnitLess) == ukUnitLess and mel != "UnitLess": true else: false
    let ctUnit = initCTUnit(buf, unitKind, exp, prefix,
                            isShortHand = isShortHand)
    result.add ctUnit
    inc idx

proc parseCTUnit(x: NimNode): CTCompoundUnit =
  if x.isUnitLessNumber:
    let ctUnit = initCTUnit(x.getTypeImpl.repr, ukUnitLess, 1, siIdentity)
    result.add ctUnit
    return result
  let xTyp = x.getUnitType
  var xT = xTyp.strVal
  ## TODO: avoid walking over `xT` so many times!
  if "•" in xT or digitsAndMinus.anyIt(it in xT):
    result = parseCTUnitUnicode(xT)
  elif "*" in xT or xT.anyIt(it in AsciiChars):
    result = parseCTUnitAscii(xT)
  # TODO: add verbose mode
  #elif "Per" in xT:
  #  result = parseCTUnitVerbose(x)
  else:
    # else does not matter which proc, because it should be a single unit, e.g. `KiloGram`
    result = parseCTUnitUnicode(xT)

proc toBaseTypeScale(u: CTUnit): float =
  result = u.siPrefix.toFactor()
  result *= u.factor
  if u.unitKind == ukGram:
    result /= 1e3 # base unit is `kg`!
  result = pow(result, u.power.float)

proc toBaseTypeScale(x: CTCompoundUnit): float =
  ## returns the scale required to turn `x` to its base type, i.e.
  ## turn all units that are not already to SI form
  # XXX: ideally we could make sure `siPrefix` is init'd to 1.0
  result = if x.siPrefix != 0.0: x.siPrefix else: 1.0 # global SI prefix as a factor
  for u in x.units:
    result *= toBaseTypeScale(u)

proc toBaseType(u: CTUnit): CTUnit =
  result = u
  case u.unitKind
  of ukGram, ukPound, ukOunce, ukSlug:
    ## SI unit base of Gram is KiloGram
    result.unitKind = ukGram # for non ukGram
    result.siPrefix = siKilo
    result.b.baseUnit = buGram
    result.b.siPrefix = siKilo
  of ukMile, ukInch, ukYard, ukFoot:
    result.siPrefix = siIdentity
    result.unitKind = ukMeter
  of ukDegree:
    result.siPrefix = siIdentity
    result.unitKind = ukSteradian
  of ukMinute, ukHour, ukDay, ukYear:
    result.siPrefix = siIdentity
    result.unitKind = ukSecond
  of ukPoundForce:
    result.siPrefix = siIdentity
    result.unitKind = ukNewton
  else: result.siPrefix = siIdentity

proc toBaseType(x: CTCompoundUnit): CTCompoundUnit =
  ## converts `x` to a unit representing the base type.
  ## WARNING: this is a lossy conversion, so make sure to extract the
  ## conversion scales using `toBaseTypeScale` before doing this!
  ## TODO: can we add to `CTUnit` a scale?
  for u in x.units:
    result.add u.toBaseType

proc sanitizeInput(n: NimNode): NimNode =
  # remove all `nnkConv, nnkHiddenStdConv and nnkStmtListExpr`
  let tree = n
  proc sanitize(n: NimNode): NimNode =
    if n.len == 0:
      #result = n
      case n.kind
      of nnkSym: result = ident(n.strVal)
      else: result = n
    else:
      case n.kind
      of nnkConv, nnkHiddenStdConv, nnkHiddenCallConv: result = n[1].sanitize
      of nnkStmtListExpr: result = n[1].sanitize
      else:
        result = newTree(n.kind)
        for ch in n:
          result.add sanitize(ch)
  result = tree.sanitize()

## TODO: we should really combine these macros somewhat?
macro `==`*[T: SomeUnit; U: SomeUnit](x: T, y: U): bool =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float == `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    xCT = xCT.toBaseType().simplify()
    yCT = yCT.toBaseType().simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    ## TODO: use almostEqual?
    result = quote do:
      (`x`.float * `xScale` == `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float < `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    xCT = xCT.toBaseType().simplify()
    yCT = yCT.toBaseType().simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    result = quote do:
      (`x`.float * `xScale` < `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<=`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float <= `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    xCT = xCT.toBaseType().simplify()
    yCT = yCT.toBaseType().simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    result = quote do:
      (`x`.float * `xScale` <= `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `+`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if xCT == yCT:
    # excactly the same type, just add
    let resType = xCT.toNimType() # same type, just use `xCT`
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float + `yr`.float)
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
      `resType`(`xr`.float * `xScale` + `yr`.float * `yScale`)
  else:
    error("Different quantities cannot be added! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `-`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseCTUnit(x)
  var yCT = parseCTUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()

  if xCT == yCT:
    # excactly the same type, just add
    let resType = xCT.toNimType() # same type, just use `xCT`
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float - `yr`.float)
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
      `resType`(`xr`.float * `xScale` - `yr`.float * `yScale`)
  else:
    error("Different quantities cannot be subtracted! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

proc `-`*[T: SomeUnit](x: T): T = (-(x.float)).T

proc `+=`*[T: SomeUnit](x: var T, y: T) =
  x = x + y

proc `-=`*[T: SomeUnit](x: var T, y: T) =
  x = x - y

proc `*=`*[T: SomeUnit](x: var T, y: UnitLess) =
  x = x * y

proc `/=`*[T: SomeUnit](x: var T, y: UnitLess) =
  x = x / y

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
      ## TODO: idea here was to accomodate auto conversion eV -> Joule I think?!
      ## Think about if we broke something outside of unit tests! Maybe implicit math with different units?
      ## Or not a problem anymore, since eV -> Joule isn't done while flattened anymore?
      # uBase.factor *= scale
      result.add uBase
    else:
      result.add u

macro `*`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  ## TODO: can we extract the actual mathy part from x, y instead of using the
  ## whole expression? And then reinsert that after our change
  var xCT = parseCTUnit(x)
  let yCT = parseCTUnit(y)
  # add `yCT` to xCT. Equates a product after simplification
  xCT.add yCT
  # TODO: automatically perform scaling to SI units?
  xCT = xCT.flatten()
  var resTypeCT = xCT.convertIfMultipleSiPrefixes()
  let scaleOriginal = xCT.toBaseTypeScale()
  let scaleConv = resTypeCT.toBaseTypeScale() ## WRONG: must not *always* call conversion
  ## TODO: check if there are multiple SI prefixes of the same units present.
  ## If so, convert to base units, else do not.
  let resType = resTypeCT.simplify().toNimType()
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if scaleOriginal != scaleConv:
    let scale = scaleOriginal / scaleConv
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float * `yr`.float * `scale`)
  else:
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float * `yr`.float)

#template `*`*[T: SomeUnit; U: SomeNumber](x: T; y: U{lit}): T = (x.float * y.float).T
#template `*`*[T: SomeUnit; U: SomeNumber](x: U{lit}; y: T): T = (x.float * y.float).T

macro `/`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseCTUnit(x)
  let yCT = parseCTUnit(y)
  # add inverted `yCT` (power -> -power) to xCT. Equates a division after simplification
  xCT.add yCT.invert()
  xCT = xCT.flatten()
  var resTypeCT = xCT.convertIfMultipleSiPrefixes()
  let scaleOriginal = xCT.toBaseTypeScale()
  let scaleConv = resTypeCT.toBaseTypeScale() ## WRONG: must not *always* call conversion
  ## TODO: check if there are multiple SI prefixes of the same units present.
  ## If so, convert to base units, else do not.
  let resType = resTypeCT.simplify().toNimType()
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if scaleOriginal != scaleConv:
    let scale = scaleOriginal / scaleConv
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float / `yr`.float * `scale`)
  else:
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float / `yr`.float)

proc commonQuantity(x: typedesc, y: typedesc): bool =
  ## checks if x and y are equivalent quantities
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
  let yCT = to
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
      defUnit(`resType`)
      `resType`(`x`.float * `scale`)
  else:
    error("Cannot convert " & $(xCT.toNimType()) & " to " & $(yCT.toNimType()) & " as they represent different " &
      "quantities!")

{.experimental: "dotOperators".}
macro `.`*[T: SomeUnit|SomeNumber](x: T; y: untyped): untyped =
  ## macro to allow to generate new types on the fly
  ## TODO: maybe have an explicit distinction between already defined types
  ## and not defined types? Use `.!` or something like this instead?
  let typX = x.getTypeInst()
  let yCT = y.parseCTUnit()
  let resType = yCT.simplify().toNimType()
  # check whether argument is actually `UnitLess` and not something that fails
  # parsing as a unit. TODO: improve unit parsing to handle this? Need a failed
  # parsing state != UnitLess
  let isUnitLess = y.strVal == "UnitLess" or
    (yCT.units.len > 1 and # > 1 means something like mol•mol⁻¹ can work
     resType.strVal == "UnitLess")
  let rewrite = not isUnitLess and resType.strVal == "UnitLess" # parsing failed
  let xr = x.sanitizeInput()
  if not rewrite:
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float)
  else:
    # parsing as a unit failed, rewrite to get possible CT error or correct result
    result = quote do:
      `y` `x`

proc baseUnitToNaturalUnit(b: CTBaseUnit): CTUnit =
  ## Converts the given base unit to a natural unit, i.e. `eV` of the corresponding
  ## power (according to the quantity).
  case b.baseUnit
  of buUnitLess:
    result = initCTUnit("", ukUnitLess, power = 1 * b.power, b.siPrefix, factor = some(1.0))
  of buGram:
    if b.siPrefix == siKilo:
      result = initCTUnit("eV", ukElectronVolt, power = 1 * b.power, siIdentity, factor = some(1.7826627e-36))
    else:
      result = initCTUnit("eV", ukElectronVolt, power = 1 * b.power, b.siPrefix, factor = some(1.7826627e-36))
  of buMeter:
    result = initCTUnit("eV⁻¹", ukElectronVolt, power = -1 * b.power, b.siPrefix, factor = some(1.9732705e-7))
  of buSecond:
    result = initCTUnit("eV⁻¹", ukElectronVolt, power = -1 * b.power, b.siPrefix, factor = some(6.5821220e-16))
  of buAmpere:
    result = initCTUnit("eV", ukElectronVolt, power = 1 * b.power, b.siPrefix, factor = some(0.00080381671))
  of buKelvin: error("Broken")
  of buMol:
    result = initCTUnit("", ukUnitLess, power = 1 * b.power, b.siPrefix, factor = some(1.0))
  of buCandela: error("Broken")
  result.factor = pow(result.factor, b.power.float)
  result.factor = 1.0 / result.factor

proc toNaturalUnitImpl(t: CTUnit): CTUnit =
  ## TODO: problem is T is converted into flattened type and mT is kept as milli tesla!!!
  ## TODO2: is this still a problem?
  case t.unitType
  of utQuantity:
    var unit = baseUnitToNaturalUnit(t.b)
    let prefixFactor = if t.unitKind == ukGram: t.siPrefix.toFactor() / 1000.0
                       else: t.siPrefix.toFactor()
    unit.factor = pow(unit.factor * prefixFactor, t.power.float)
    unit.power *= t.power
    result = unit
  of utCompoundQuantity:
    ## NOTE: this should be a single eV CT unit. Need to merge factors
    result = initCTUnit("eV", ukElectronVolt, power = 0, siPrefix = siIdentity)
    for b in t.bs:
      var unit = baseUnitToNaturalUnit(b)
      result.factor *= unit.factor
      result.power += unit.power
    result.factor = pow(result.factor * t.siPrefix.toFactor(), t.power.float)

proc toNaturalUnitImpl(t: CTCompoundUnit): CTCompoundUnit =
  ## Converts a compound unit to natural units
  for unit in t.units:
    result.add toNaturalUnitImpl(unit)

proc toNaturalScale(t: CTCompoundUnit): float =
  ## Returns the scaling factor associated with a unit converted
  ## to natural units
  result = 1.0
  for unit in t.units:
    result *= unit.factor

macro toNaturalUnit*[T: SomeUnit](t: T): untyped =
  ## parses the unit and converts it to natural units (`eV`) according to
  ## the contained
  var typ = t.parseCTUnit()
    .toNaturalUnitImpl()
  let scale = typ.toNaturalScale()
  let resType = typ.simplify().toNimType()
  result = quote do:
    defUnit(`resType`)
    `resType`(`t`.float * `scale`)

macro sqrt*[T: SomeUnit](t: T): untyped =
  ## Implements the `sqrt` of a given unitful value.
  ##
  ## Fails if the given unit is not a perfect square (i.e. each compound of the full
  ## unit's power is a multiple of 2).
  let typ = t.parseCTUnit()

  var mType = typ
  for u in mitems(mType.units):
    if u.power mod 2 == 0: # can be divided
      u.power = u.power div 2
    else:
      error("Cannot take the `sqrt` of input unit " & $(typ.toNimType()) & " as it's not a perfect square!")
  let resType = mType.toNimType()
  let tr = t.sanitizeInput()
  result = quote do:
    # defUnit(`resType`) # Should not be needed. The sqrt (if valid) will be a known type
    `resType`(sqrt(`tr`.float))

proc abs*[T: SomeUnit](t: T): T = (abs(t.float)).T
