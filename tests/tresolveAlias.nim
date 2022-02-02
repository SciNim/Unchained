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
