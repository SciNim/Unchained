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
    Force:                 [(Length, 1), (Mass, 1), (Time, -2)]
    Energy:                [(Mass, 1), (Length, 2), (Time, -2)]
    ElectricPotential:     [(Mass, 1), (Length, 2), (Time, -3), (Current, -1)]
    # XXX: allow to define aliases here? Voltage: ElectricPotential ?
    Charge:                [(Time, 1), (Current, 1)]
    Power:                 [(Length, 2), (Mass, 1), (Time, -3)]
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
      short: G
      quantity: MagneticFieldStrength
      conversion: 1e-4.T # non SI defined by having a conversion

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
      conversion: 31536000.s # 365.0 * 86400.0, could also use ~365.25...

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

## Converters to help make Radian and Steradian more convenient. Will be
## generated in the future from the `declareUnit` macro.
converter toRawFloat*(x: Radian): float = x.float
converter toRawFloat*(x: Steradian): float = x.float
converter toRadian*(x: float): Radian = x.Radian
converter toSteradian*(x: float): Steradian = x.Steradian
