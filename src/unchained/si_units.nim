## Import the CT API to generate quantites & units
import ct_api

## Generate the types for the base quantities and their derived quantities.
## Also generates the `QuantityKind` enum, which is simply an enum of all quantities
## listed here prefixed by `qk` in the order given here. The `qkUnitLess` element
## is always added automatically.
declareQuantities:
  Base:
    Time
    Length
    Mass
    Current
    Temperature
    AmountOfSubstance
    Luminosity
  Derived:
    Frequency:             [(Time, -1)]
    Velocity:              [(Length, 1), (Time, -1)]
    Acceleration:          [(Length, 1), (Time, -2)]
    Area:                  [(Length, 2)]
    Volume:                [(Length, 3)]
    Momentum:              [(Mass, 1), (Length, 1), (Time, -1)]
    Force:                 [(Mass, 1), (Length, 1), (Time, -2)]
    Energy:                [(Mass, 1), (Length, 2), (Time, -2)]
    Torque:                [(Mass, 1), (Length, 2), (Time, -2)]
    ElectricPotential:     [(Mass, 1), (Length, 2), (Time, -3), (Current, -1)]
    # XXX: allow to define aliases here? Voltage: ElectricPotential ?
    Charge:                [(Time, 1), (Current, 1)]
    Power:                 [(Mass, 1), (Length, 2), (Time, -3)]
    ElectricResistance:    [(Mass, 1), (Length, 2), (Time, -3), (Current, -2)]
    Inductance:            [(Mass, 1), (Length, 2), (Time, -2), (Current, -2)]
    Capacitance:           [(Mass, -1), (Length, -2), (Time, 4), (Current, 2)]
    Pressure:              [(Mass, 1), (Length, -1), (Time, -2)]
    Density:               [(Mass, 1), (Length, -3)]
    Angle:                 [(Length, 1), (Length, -1)]
    SolidAngle:            [(Length, 2), (Length, -2)]
    MagneticFieldStrength: [(Mass, 1), (Time, -2), (Current, -1)]
    Activity:              [(Time, -1)]
    AreaDensity:           [(Length, -2), (Mass, 1)]

# generate the concepts for the quantities to use them as types in procedures
generateQuantityConcepts()

## Define units is imported only *after* the quantities are declared!
#import define_units
export define_units.commonQuantity

## Generate all base types (base units representing the defined quantities) and
## possible other (compound) types that are defined via a conversion to an existing
## base type.

declareUnits:
  BaseUnits: # SI base units
    Gram:
      short: g
      prefix: siKilo # the actual SI unit is `kg` instead of `g`.
      quantity: Mass
    Meter:
      short: m
      quantity: Length
    Ampere:
      short: A
      quantity: Current
    Second:
      short: s
      quantity: Time
    Kelvin:
      short: K
      quantity: Temperature
    Mol:
      short: mol
      quantity: AmountOfSubstance
    Candela:
      short: cd
      quantity: Luminosity

  Derived:
    Newton:
      short: N
      quantity: Force # generate SI based derivative based on Quantity & quantity dimensionality
    Joule:
      short: J
      quantity: Energy
    Volt:
      short: V
      quantity: ElectricPotential
    Hertz:
      short: Hz
      quantity: Frequency
    Coulomb:
      short: C
      quantity: Charge
    Watt:
      short: W
      quantity: Power
    Ohm:
      short: Ω
      quantity: ElectricResistance
    Henry:
      short: H
      quantity: Inductance
    Farad:
      short: F
      quantity: Capacitance
    Pascal:
      short: Pa
      quantity: Pressure
    Tesla:
      short: T
      quantity: MagneticFieldStrength
    Becquerel:
      short: Bq
      quantity: Activity

    # angle based units. These are technically UnitLess, hence we forbid auto conversion
    Radian:
      short: rad
      quantity: Angle
      autoConvert: false # do not auto convert radian (would drop information) in `flatten` calls
    Steradian:
      short: sr
      quantity: SolidAngle
      autoConvert: false # do not auto convert steradian (would drop information)

    # Non SI units
    Gauss:
      short: gauss
      quantity: MagneticFieldStrength
      conversion: 1e-4.T # non SI defined by having a conversion

    Torr:
      short: torr
      quantity: Pressure
      conversion: 133.322368421.Pa # 101325.0 / 760.0 <-> (1 atm / 760 Torr)

    Erg:
      short: erg
      quantity: Energy
      conversion: 1e-7.J

    AstronomicalUnit:
      short: AU
      quantity: Length
      conversion: 149_597_870_700.0.m # by definition since 2012

    LightYear:
      short: ly
      quantity: Length
      conversion: 9_460_730_472_580_800.0.m # 365.25 * 86400 * c_0

    Parsec:
      short: parsec
      quantity: Length
      conversion: 30856775814913673.0.m # 1 AU * 180° * 60 * 60 / π

    Dalton: # unified atomic mass, m_u = 1/12 m(¹²C) = 1 Da.
      short: Da
      quantity: Mass
      conversion: 1.66053906892e-27.kg # 1.66053906892(52)×10−27

    # given that we have base units & derived base units defined, we can now just
    # dump everything together. Everything that is referenced before, can now be
    # used to define new units.
    # other units
    ElectronVolt:
      short: eV
      quantity: Energy
      conversion: 1.602176634e-19.J
      # autoConvert: false # use auto convert?
    Bar:
      short: bar
      quantity: Pressure
      conversion: 100_000.Pa
    Liter:
      short: L
      quantity: Volume
      conversion: 1e-3.m³
    Degree:
      short: °
      quantity: Angle
      conversion: 0.0174532925199.rad # PI / 180.0
    ArcMinute:
      short: arcmin
      quantity: Angle
      conversion: 0.000290888.rad # 1.° / 60.0 in Radian
    ArcSecond:
      short: arcsec
      quantity: Angle
      conversion: 0.0000048481368111.rad # 1.° / (60 * 60) in Radian

    # Area commonly used for cross sections in atomic / particle physics
    Barn:
      short: barn # `b` is too ambiguous with prefixes
      quantity: Area
      conversion: 1e-28.m²

    Minute:
      short: min
      quantity: Time
      conversion: 60.0.s
    Hour:
      short: h
      quantity: Time
      conversion: 3600.0.s
    Day:
      short: day
      quantity: Time
      conversion: 86400.0.s
    Year:
      short: yr
      quantity: Time
      conversion: 31557600.s # 365.25 * 86400.0 (Julian year) (to match definition of lightyear)

    # Imperial
    Pound:
      short: lbs
      quantity: Mass
      conversion: 0.45359237.kg
    Mile:
      short: mi
      quantity: Length
      conversion: 1609.344.m
    Inch:
      short: inch
      quantity: Length
      conversion: 0.0254.m
    Foot:
      short: ft
      quantity: Length
      conversion: 0.3048.m
    Yard:
      short: yd
      quantity: Length
      conversion: 0.9144.m
    Ounce:
      short: oz
      quantity: Mass
      conversion: 28.349523125e-3.kg
    Slug:
      short: slug
      quantity: Mass
      conversion: 14.59390294.kg
    Acre:
      short: acre
      quantity: Area
      conversion: 4046.8564224.m²
    PoundForce:
      short: lbf
      quantity: Force
      conversion: 4.44822162.N

  # definition of the conversions to natural units for all base units
  # takes place here, because at definition of the base units the `eV` unit
  # is not defined yet.
  # Lorentz-Heaviside convention, c = ε_0 = hbar = k_B = 1, but explicitly
  # no mass (electron or proton) is set to 1, nor do we swallow any factors
  # of 4π!
  NaturalUnits:
    Gram: 1.7826627e-33.eV # relative to g and not kg!
    Meter: 1.9732705e-7.eV⁻¹
    Ampere: 0.00080381671.eV
    Second: 6.5821220e-16.eV⁻¹
    Kelvin: 11604.518.eV
    Mol: 1.0
    Candela: 1.0

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
  (T, Tesla) exclude [f] # fT would be ambiguous with `ft` (foot)
  (Bq, Becquerel)
  (gauss, Gauss)
  (barn, Barn)

## Converters to help make Radian and Steradian more convenient. Will be
## generated in the future from the `declareUnit` macro.
converter toRawFloat*(x: Radian): FloatType = x.FloatType
converter toRawFloat*(x: Steradian): FloatType = x.FloatType
converter toRadian*(x: FloatType): Radian = x.Radian
converter toSteradian*(x: FloatType): Steradian = x.Steradian
