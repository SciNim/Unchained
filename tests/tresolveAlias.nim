import unchained / units
import macros
import unittest

type
  InvalidObject = object
  InvalidRef = ref object
  InvalidAlias = InvalidObject
  InvalidPtr = ptr InvalidObject
  InvalidDistinct = distinct InvalidObject

suite "Resolve valid units":
  template checkUnit(arg: typed): untyped =
    test "Valid: " & $astToStr(arg):
      check isAUnit(arg)

  checkUnit(Unit)
  checkUnit(Quantity)
  checkUnit(CompoundQuantity)
  checkUnit(UnitLess)

  ## Base Quantities
  checkUnit(Time)
  checkUnit(Length)
  checkUnit(Mass)
  checkUnit(Current)
  checkUnit(Temperature)
  checkUnit(AmountOfSubstance)
  checkUnit(Luminosity)

  checkUnit(BaseQuantity)

  ## Derived quantities, TODO: should be `distinct CompoundQuantity`? Not a single dimension!
  checkUnit(Velocity)
  checkUnit(Acceleration)
  checkUnit(Momentum)
  checkUnit(Force)
  checkUnit(Energy)
  checkUnit(Density)

  checkUnit(ElectricPotential)
  checkUnit(Voltage)

  checkUnit(Frequency)

  checkUnit(Charge)
  checkUnit(Power)
  checkUnit(ElectricResistance)
  checkUnit(Capacitance)
  checkUnit(Inductance)
  checkUnit(Pressure)

  # angles and solid angles are technically UnitLess.
  checkUnit(Angle)
  checkUnit(SolidAngle)

  checkUnit(DerivedQuantity)

  checkUnit(SomeQuantity)

  ## Base SI units
  checkUnit(Second)
  checkUnit(Meter)
  checkUnit(Gram)
  checkUnit(KiloGram)
  checkUnit(Ampere)
  checkUnit(Kelvin)
  checkUnit(Mol)
  checkUnit(Candela)

  checkUnit(SiUnit)

  ## compound units, i.e. definition of different physical concepts.
  checkUnit(KiloGram•Meter•Second⁻¹)
  checkUnit(Second²)
  checkUnit(Meter•Second⁻¹)
  checkUnit(Meter•Second⁻²)
  checkUnit(KiloGram•Meter²•Second⁻²)
  checkUnit(KiloGram•Meter•Second⁻²)
  checkUnit(Second⁻¹)
  checkUnit(KiloGram•Meter²•Second⁻³)
  checkUnit(Ampere•Second)
  checkUnit(KiloGram•Meter²•Second⁻²•Ampere⁻²)
  checkUnit(Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹)
  checkUnit(KiloGram•Meter²•Ampere⁻¹•Second⁻³)
  checkUnit(KiloGram•Meter²•Second⁻³•Ampere⁻²)
  checkUnit(KiloGram•Meter⁻¹•Second⁻²)
  checkUnit(KiloGram•Meter⁻³)
  checkUnit(Meter•Meter⁻¹)
  checkUnit(Meter²•Meter⁻²)

  ## derived SI units
  checkUnit(Newton)
  checkUnit(Joule)
  checkUnit(Volt)
  checkUnit(Hertz)
  checkUnit(Coulomb)
  checkUnit(Watt)
  checkUnit(Ohm)
  checkUnit(Henry)
  checkUnit(Farad)
  checkUnit(Pascal)
  checkUnit(Radian)
  checkUnit(Steradian)
  checkUnit(Tesla)
  checkUnit(Becquerel)

  ## other units
  checkUnit(ElectronVolt)
  checkUnit(Bar)
  checkUnit(Degree)
  checkUnit(Minute)
  checkUnit(Hour)
  checkUnit(Day)
  checkUnit(Year)

  checkUnit(Joule•Coulomb⁻¹)
  checkUnit(Ampere•Ohm)

  checkUnit(DerivedSiUnits)

  ## shorthand types
  checkUnit(m)
  checkUnit(s)
  checkUnit(A)
  checkUnit(mol)
  checkUnit(m•s⁻²)
  checkUnit(meterPerSecondSquared)
  checkUnit(g)
  checkUnit(Kg)
  checkUnit(kg)
  checkUnit(N)
  checkUnit(V)
  checkUnit(Hz)
  checkUnit(J)
  checkUnit(C)
  checkUnit(W)
  checkUnit(Ω)
  checkUnit(H)
  checkUnit(F)
  checkUnit(eV)
  checkUnit(Pa)
  checkUnit(bar)
  checkUnit(g•cm⁻³)
  checkUnit(rad)
  checkUnit(sr)
  checkUnit(°)
  checkUnit(units.min)
  checkUnit(h)
  checkUnit(day)
  checkUnit(yr)

suite "Resolve user defined units":
  test "Valid user defined units":
    defUnit(A•J•m²•s⁻⁵)
    check isAUnit(A•J•m²•s⁻⁵)
    check isAUnit(Ampere)
    let x = 5.A
    let y = 2.s
    let z = x * y
    ## NOTE: `A•s` is not created!
    check isAUnit(Ampere•Second)

suite "Valid unit-ful values":
  test "Valid units from a generic":
    proc foo[T](arg: T) =
      check isAUnit(T)

    template wrapFoo(typ: typed): untyped =
      var tmp: typ
      foo(tmp)

    wrapFoo(Unit)
    wrapFoo(Quantity)
    wrapFoo(CompoundQuantity)
    wrapFoo(UnitLess)

    ## Base Quantities
    wrapFoo(Time)
    wrapFoo(Length)
    wrapFoo(Mass)
    wrapFoo(Current)
    wrapFoo(Temperature)
    wrapFoo(AmountOfSubstance)
    wrapFoo(Luminosity)

    ## Derived quantities, TODO: should be `distinct CompoundQuantity`? Not a single dimension!
    wrapFoo(Velocity)
    wrapFoo(Acceleration)
    wrapFoo(Momentum)
    wrapFoo(Force)
    wrapFoo(Energy)
    wrapFoo(Density)

    wrapFoo(ElectricPotential)
    wrapFoo(Voltage)

    wrapFoo(Frequency)

    wrapFoo(Charge)
    wrapFoo(Power)
    wrapFoo(ElectricResistance)
    wrapFoo(Capacitance)
    wrapFoo(Inductance)
    wrapFoo(Pressure)

    #  angles are technically UnitLess.
    wrapFoo(Angle)
    wrapFoo(SolidAngle)

    ## Base SI units
    wrapFoo(Second)
    wrapFoo(Meter)
    wrapFoo(Gram)
    wrapFoo(KiloGram)
    wrapFoo(Ampere)
    wrapFoo(Kelvin)
    wrapFoo(Mol)
    wrapFoo(Candela)

    ## compound units, i.e. definition of different physical concepts.
    wrapFoo(KiloGram•Meter•Second⁻¹)
    wrapFoo(Second²)
    wrapFoo(Meter•Second⁻¹)
    wrapFoo(Meter•Second⁻²)
    wrapFoo(KiloGram•Meter²•Second⁻²)
    wrapFoo(KiloGram•Meter•Second⁻²)
    wrapFoo(Second⁻¹)
    wrapFoo(KiloGram•Meter²•Second⁻³)
    wrapFoo(Ampere•Second)
    wrapFoo(KiloGram•Meter²•Second⁻²•Ampere⁻²)
    wrapFoo(Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹)
    wrapFoo(KiloGram•Meter²•Ampere⁻¹•Second⁻³)
    wrapFoo(KiloGram•Meter²•Second⁻³•Ampere⁻²)
    wrapFoo(KiloGram•Meter⁻¹•Second⁻²)
    wrapFoo(KiloGram•Meter⁻³)
    wrapFoo(Meter•Meter⁻¹)
    wrapFoo(Meter²•Meter⁻²)

    ## derived SI units
    wrapFoo(Newton)
    wrapFoo(Joule)
    wrapFoo(Volt)
    wrapFoo(Hertz)
    wrapFoo(Coulomb)
    wrapFoo(Watt)
    wrapFoo(Ohm)
    wrapFoo(Henry)
    wrapFoo(Farad)
    wrapFoo(Pascal)
    wrapFoo(Radian)
    wrapFoo(Steradian)
    wrapFoo(Tesla)
    wrapFoo(Becquerel)

    ## other units
    wrapFoo(ElectronVolt)
    wrapFoo(Bar)
    wrapFoo(Degree)
    wrapFoo(Minute)
    wrapFoo(Hour)
    wrapFoo(Day)
    wrapFoo(Year)

    wrapFoo(Joule•Coulomb⁻¹)
    wrapFoo(Ampere•Ohm)

    ## shorthand types
    wrapFoo(m)
    wrapFoo(s)
    wrapFoo(A)
    wrapFoo(mol)
    wrapFoo(m•s⁻²)
    wrapFoo(meterPerSecondSquared)
    wrapFoo(g)
    wrapFoo(Kg)
    wrapFoo(kg)
    wrapFoo(N)
    wrapFoo(V)
    wrapFoo(Hz)
    wrapFoo(J)
    wrapFoo(C)
    wrapFoo(W)
    wrapFoo(Ω)
    wrapFoo(H)
    wrapFoo(F)
    wrapFoo(eV)
    wrapFoo(Pa)
    wrapFoo(bar)
    wrapFoo(g•cm⁻³)
    wrapFoo(rad)
    wrapFoo(sr)
    wrapFoo(°)
    wrapFoo(units.min)
    wrapFoo(h)
    wrapFoo(day)
    wrapFoo(yr)

suite "Invalid units":
  test "Invalid units":
    check not isAUnit(float)
    check not isAUnit(int)
    check not isAUnit(string)
    check not isAUnit(seq[string])
    check not isAUnit(seq[float])
    check not isAUnit(InvalidObject)
    check not isAUnit(InvalidRef)
    check not isAUnit(InvalidAlias)
    check not isAUnit(InvalidDistinct)
    check not isAUnit(InvalidPtr)

  test "Invalid units from a generic":

    proc foo[T](arg: T) =
      check not isAUnit(T)

    template wrapFoo(typ: typed): untyped =
      var tmp: typ
      foo(tmp)

    wrapfoo(float)
    wrapfoo(int)
    wrapfoo(string)
    wrapfoo(seq[string])
    wrapfoo(seq[float])
    wrapfoo(InvalidObject)
    wrapfoo(InvalidRef)
    wrapfoo(InvalidAlias)
    wrapfoo(InvalidDistinct)
    wrapfoo(InvalidPtr)
