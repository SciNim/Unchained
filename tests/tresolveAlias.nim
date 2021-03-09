import unchained / units
import macros
import unittest

type
  InvalidObject = object
  InvalidRef = ref object
  InvalidAlias = InvalidObject
  InvalidPtr = ptr InvalidObject
  InvalidDistinct = distinct InvalidObject

suite "Resolve unit aliases":
  test "Valid units":
    check isAUnit(Unit)
    check isAUnit(Quantity)
    check isAUnit(CompoundQuantity)
    check isAUnit(UnitLess)

    ## Base Quantities
    check isAUnit(Time)
    check isAUnit(Length)
    check isAUnit(Mass)
    check isAUnit(Current)
    check isAUnit(Temperature)
    check isAUnit(AmountOfSubstance)
    check isAUnit(Luminosity)

    check isAUnit(BaseQuantity)

    ## Derived quantities, TODO: should be `distinct CompoundQuantity`? Not a single dimension!
    check isAUnit(Velocity)
    check isAUnit(Acceleration)
    check isAUnit(Momentum)
    check isAUnit(Force)
    check isAUnit(Energy)
    check isAUnit(Density)

    check isAUnit(ElectricPotential)
    check isAUnit(Voltage)

    check isAUnit(Frequency)

    check isAUnit(Charge)
    check isAUnit(Power)
    check isAUnit(ElectricResistance)
    check isAUnit(Capacitance)
    check isAUnit(Inductance)
    check isAUnit(Pressure)

    # angles and solid angles are technically UnitLess.
    check isAUnit(Angle)
    check isAUnit(SolidAngle)

    check isAUnit(DerivedQuantity)

    check isAUnit(SomeQuantity)

    ## Base SI units
    check isAUnit(Second)
    check isAUnit(Meter)
    check isAUnit(Gram)
    check isAUnit(KiloGram)
    check isAUnit(Ampere)
    check isAUnit(Kelvin)
    check isAUnit(Mol)
    check isAUnit(Candela)

    check isAUnit(SiUnit)

    ## compound units, i.e. definition of different physical concepts.
    check isAUnit(KiloGram•Meter•Second⁻¹)
    check isAUnit(Second²)
    check isAUnit(Meter•Second⁻¹)
    check isAUnit(Meter•Second⁻²)
    check isAUnit(KiloGram•Meter²•Second⁻²)
    check isAUnit(KiloGram•Meter•Second⁻²)
    check isAUnit(Second⁻¹)
    check isAUnit(KiloGram•Meter²•Second⁻³)
    check isAUnit(Ampere•Second)
    check isAUnit(KiloGram•Meter²•Second⁻²•Ampere⁻²)
    check isAUnit(Second⁴•Ampere²•Meter⁻²•KiloGram⁻¹)
    check isAUnit(KiloGram•Meter²•Ampere⁻¹•Second⁻³)
    check isAUnit(KiloGram•Meter²•Second⁻³•Ampere⁻²)
    check isAUnit(KiloGram•Meter⁻¹•Second⁻²)
    check isAUnit(KiloGram•Meter⁻³)
    check isAUnit(Meter•Meter⁻¹)
    check isAUnit(Meter²•Meter⁻²)

    ## derived SI units
    check isAUnit(Newton)
    check isAUnit(Joule)
    check isAUnit(Volt)
    check isAUnit(Hertz)
    check isAUnit(Coulomb)
    check isAUnit(Watt)
    check isAUnit(Ohm)
    check isAUnit(Henry)
    check isAUnit(Farad)
    check isAUnit(Pascal)
    check isAUnit(Radian)
    check isAUnit(Steradian)

    ## other units
    check isAUnit(ElectronVolt)
    check isAUnit(Bar)
    check isAUnit(Degree)
    check isAUnit(Minute)
    check isAUnit(Hour)
    check isAUnit(Day)
    check isAUnit(Year)

    check isAUnit(Joule•Coulomb⁻¹)
    check isAUnit(Ampere•Ohm)

    check isAUnit(DerivedSiUnits)

    ## shorthand types
    check isAUnit(m)
    check isAUnit(s)
    check isAUnit(A)
    check isAUnit(mol)
    check isAUnit(m•s⁻²)
    check isAUnit(meterPerSecondSquared)
    check isAUnit(g)
    check isAUnit(Kg)
    check isAUnit(kg)
    check isAUnit(N)
    check isAUnit(V)
    check isAUnit(Hz)
    check isAUnit(J)
    check isAUnit(C)
    check isAUnit(W)
    check isAUnit(Ω)
    check isAUnit(H)
    check isAUnit(F)
    check isAUnit(eV)
    check isAUnit(Pa)
    check isAUnit(bar)
    check isAUnit(g•cm⁻³)
    check isAUnit(rad)
    check isAUnit(sr)
    check isAUnit(°)
    check isAUnit(units.min)
    check isAUnit(h)
    check isAUnit(day)
    check isAUnit(yr)

  test "Valid user defined":
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
