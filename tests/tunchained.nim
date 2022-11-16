import unchained
import unittest

template fails(x: untyped): untyped =
  when compiles(x):
    false
  else:
    true

import math, fenv
func almostEq(a, b: float, epsilon = 1e-8): bool =
  # taken from
  # https://floating-point-gui.de/errors/comparison/
  let
    absA = abs(a)
    absB = abs(b)
    diff = abs(a - b)
  if a == b: # shortcut, handles infinities
    result = true
  elif a == 0 or b == 0 or (absA + absB) < minimumPositiveValue(float64):
    # a or b is zero or both are extremely close to it
    # relative error is less meaningful here
    result = diff < (epsilon * minimumPositiveValue(float64))
  else:
    # use relative error
    result = diff / min(absA + absB, maximumPositiveValue(float64)) < epsilon

proc `=~=`[T: SomeUnit|UnitLess; U: SomeUnit|UnitLess](a: T; b: U): bool =
  when T is U:
    result = almostEq(a.float, b.float, epsilon = 1e-5) and type(a) is type(b)
  elif commonQuantity(T, U):
    result = almostEq(a.float, b.float, epsilon = 1e-5) and type(a) is type(b)
  else:
    {.error: "Given type " & $T & " and " & $U & " have different quantities.".}

suite "Unchained - Basic definitions":
  test "Simple type definitions":
    let a = 9.81.m•s⁻²
    let b = 4.2
    let c = b.m•s⁻²
    let mass = 12.kg
    check typeof(a) is Meter•Second⁻²
    check typeof(c) is Meter•Second⁻²
    check typeof(mass) is KiloGram

  test "Shorthand types are equal to long form types":
    let a = 10.kg
    let b = 10.KiloGram
    let c = 9.81.m•s⁻²
    let d = 9.81.Meter•Second⁻²
    check a == b
    check c == d

suite "Unchained - UnitLess ⇔ floats":
  test "Auto conversion from UnitLess ⇒ float":
    let a = 10.UnitLess
    var b = 10.0.float
    b = b + a
    check b == 20.0

  test "Auto conversion from float ⇒ UnitLess":
    let a = 10.0.float
    var b = 10.0.UnitLess
    b = b + a
    check b == 20.0.UnitLess

suite "Unchained - Basic unit math":
  test "Math: `+` of units - same quantity and SI prefix":
    let a = 10.kg
    let b = 5.kg
    check typeof(a + b) is KiloGram
    check a + b == 15.kg

  test "Math: `+` of units - same quantity, different SI prefix, auto conversion to base":
    block:
      let a = 10.kg
      let b = 5000.g
      check typeof(a + b) is KiloGram
      check a + b == 15.kg
    block:
      let a = 10_000_000.mg
      let b = 5000.g
      check typeof(a + b) is KiloGram
      check a + b == 15.kg

  test "Math: `+` of units - same quantity, different (non SI) units, conversion to SI":
    block:
      let a = 5.lbs
      let b = 2.kg
      check typeof(a + b) is KiloGram
      check typeof(b + a) is KiloGram
      check a + b =~= 4.267961.kg
      check b + a =~= 4.267961.kg
    block:
      let a = 5.Minute
      let b = 20.Second
      check typeof(a + b) is Second
      check typeof(b + a) is Second
      check a + b == 320.Second
      check b + a == 320.Second
    block:
      let a = 1.Minute
      let b = 1.Hour
      check typeof(a + b) is Second
      check typeof(b + a) is Second
      check a + b == 3660.Second
      check b + a == 3660.Second

  test "Math: `+` of units - explicit and implicit units can be added":
    let expl = 5.N
    let impl = 10.kg•m•s⁻²
    check typeof(expl + impl) is Newton
    check expl + impl == 15.Newton

  test "Math: `+` of units - different quantities cannot be added":
    let a = 10.kg
    let b = 5.m
    check fails(a + b)

    let expl = 5.N
    let impl = 10.kg•m•s⁻³
    check fails(expl + impl)

  test "Math: `+` of units - adding UnitLess and int results in float":
    ## this was a regression.
    let x = 10.UnitLess
    let y = 12
    let z = 12.5
    check x + y == 22.UnitLess
    check y + x == 22.UnitLess
    check x + z == 22.5.UnitLess
    check z + x == 22.5.UnitLess

  test "Math: `-` of units - same quantity and SI prefix":
    let a = 10.kg
    let b = 5.kg
    check typeof(a - b) is KiloGram
    check a - b == 5.kg

  test "Math: `-` of units - same quantity, different SI prefix, auto conversion to base":
    block:
      let a = 10.kg
      let b = 5000.g
      check typeof(a - b) is KiloGram
      check a - b == 5.kg
    block:
      let a = 10_000_000.mg
      let b = 5000.g
      check typeof(a - b) is KiloGram
      check a - b == 5.kg

  test "Math: `-` of units - same quantity, different (non SI) units, conversion to SI":
    block:
      let a = 5.lbs
      let b = 2.kg
      check typeof(a -  b) is KiloGram
      check typeof(b - a) is KiloGram
      check a - b =~= 0.267961.kg
      check b - a =~= -0.267961.kg
    block:
      let a = 5.Minute
      let b = 20.Second
      check typeof(a - b) is Second
      check typeof(b - a) is Second
      check a - b == 280.Second
      check b - a == -280.Second
    block:
      let a = 1.Minute
      let b = 1.Hour
      check typeof(a - b) is Second
      check typeof(b - a) is Second
      check a - b == -3540.Second
      check b - a == 3540.Second

  test "Math: `-` of units - explicit and implicit units can be subtracted":
    let expl = 10.N
    let impl = 5.kg•m•s⁻²
    check typeof(expl - impl) is Newton
    check expl - impl == 5.Newton

  test "Math: `-` of units - different quantities cannot be subtracted":
    let a = 10.kg
    let b = 5.m
    check fails(a - b)

    let expl = 10.N
    let impl = 5.kg•m•s⁻³
    check fails(expl - impl)

  test "Math: `-` of units - subtracting UnitLess and int results in float":
    ## this was a regression.
    let x = 12.UnitLess
    let y = 10
    let z = 10.5
    check x - y == 2.UnitLess
    check y - x == -2.UnitLess
    check x - z == 1.5.UnitLess
    check z - x == -1.5.UnitLess

  test "Math: `*` of units - same quantity and SI prefix":
    let a = 10.m
    let b = 2.m
    check typeof(a * b) is Meter²
    check a * b == 20.m²

  test "Math: `*` of units - same quantity, different SI prefix, auto conversion to base":
    let a = 10_000.mm
    let b = 2.m
    check typeof(a * b) is Meter²
    check a * b == 20.m²

  test "Math: `*` product of Unit and its inverse produce UnitLess":
    let a = 5.m
    let b = 5.m⁻¹
    check typeof(a * b) is UnitLess
    check a * b == 25.UnitLess

  test "Math: `/` of units - same quantity and SI prefix":
    let a = 20.m
    let b = 2.m
    check typeof(a / b) is UnitLess
    check a / b == 10.UnitLess

  test "Math: `/` of units - same quantity, different SI prefix, auto conversion to base":
    let a = 20_000.mm
    let b = 2.m
    check typeof(a / b) is UnitLess
    check a / b == 10.UnitLess

  test "Math: `*` with non base units w/o automatic conversion":
    defUnit(g•mol⁻¹)
    defUnit(mm•mol⁻¹)
    let A = 39.95.UnitLess
    let M_u = 0.99999999965e-3.kg•mol⁻¹
    check A * M_u =~= 0.03995.kg•mol⁻¹
    check A * M_u.to(g•mol⁻¹) =~= 39.95.g•mol⁻¹
    # unrelated to special case of `g/kg`:
    check A * 1e-3.mm•mol⁻¹ =~= 0.03995.mm•mol⁻¹

  test "Math: `/` with non base units w/o automatic conversion":
    defUnit(g•mol⁻¹)
    defUnit(mm•mol⁻¹)
    let A = 39.95.UnitLess
    let M_u = 0.99999999965e-3.kg•mol⁻¹
    check A / M_u =~= 39950.kg⁻¹•mol
    check A / M_u.to(g•mol⁻¹) =~= 39.95.g⁻¹•mol
    # unrelated to special case of `g/kg`:
    check A / 1e-3.mm•mol⁻¹ =~= 39950.mm⁻¹•mol

suite "Unchained - Math with compound units":

  test "Math: `+` adding units w/ `autoConvert = false` still converts if different units used":
    ## We *do* convert units with `autoConvert = false` to base units in case we perform math with
    ## other base units.
    let x = 1.Liter
    let y = 1.m³
    check type(x + y) is Meter³
    check type(y + x) is Meter³
    check x + y =~= 1.001.Meter³
    check y + x =~= 1.001.Meter³

  ## Conversion in the following tests refers to conversions of compound units to the flattened
  ## base representation, i.e. from Newton to KiloGram•Meter•Second⁻²
  test "Math: `+` adding units w/ `autoConvert = true` does *not* convert if same unit":
    let f1 = 10.mN
    let f2 = 0.5.N
    check type(f1 + f2) is Newton
    check f1 + f2 =~= 0.51.N

  test "Math: `-` subtracting units w/ `autoConvert = true` does *not* convert if same unit":
    let f1 = 10.mN
    let f2 = 0.5.N
    check type(f1 - f2) is Newton
    check f1 - f2 =~= -0.49.N

  test "Math: `*` multiplying units w/ `autoConvert = true` does *not* convert if same unit & prefix":
    let f1 = 10.mN
    let f2 = 10.mN
    defUnit(MilliNewton²)
    check type(f1 * f2) is MilliNewton²
    check f1 * f2 =~= 100.MilliNewton²

  test "Math: `*` multiplying units w/ `autoConvert = true` does *not* convert if same unit & different prefix":
    let f1 = 10.mN
    let f2 = 1.N
    defUnit(Newton²)
    check type(f1 * f2) is Newton²
    check f1 * f2 =~= 0.01.Newton²

  test "Math: `/` dividing units w/ `autoConvert = true` does *not* convert if same unit":
    let f1 = 10.mN²
    let f2 = 10.mN
    let f3 = 1.N
    check type(f1 / f2) is MilliNewton
    check f1 / f2 =~= 1.MilliNewton
    check type(f1 / f3) is Newton
    check f1 / f3 =~= 1e-5.Newton

  test "Math: `+` adding units w/ `autoConvert = false` does *not* convert if same unit":
    let f1 = 10.mL
    let f2 = 0.5.L
    check type(f1 + f2) is Liter
    check f1 + f2 =~= 0.51.L

  test "Math: `-` subtracting units w/ `autoConvert = false` does *not* convert if same unit":
    let f1 = 10.mL
    let f2 = 0.5.L
    check type(f1 - f2) is Liter
    check f1 - f2 =~= -0.49.L

  test "Math: `*` multipying units w/ `autoConvert = false` does *not* convert if same unit":
    let f1 = 10.mL
    let f2 = 10.mL
    let f3 = 1.L
    defUnit(MilliLiter²)
    defUnit(Liter²)
    check type(f1 * f2) is MilliLiter²
    check f1 * f2 =~= 100.MilliLiter²
    check type(f1 * f3) is Liter²
    check f1 * f3 =~= 0.01.Liter²

  test "Math: `/` dividing units w/ `autoConvert = false` does *not* convert if same unit":
    let f1 = 10.mL²
    let f2 = 10.mL
    let f3 = 1.L
    check type(f1 / f2) is MilliLiter
    check f1 / f2 =~= 1.MilliLiter
    check type(f1 / f3) is Liter
    check f1 / f3 =~= 1e-5.Liter

suite "Unchained - Comparisons of units":
  test "Comparisons: `<` for units of same type":
    let x = 5.kg
    let y = 10.kg
    check x < y == true
    check fails(5.kg < 10)

  test "Comparisons: `>` for units of same type":
    let x = 5.kg
    let y = 10.kg
    check x > y == false
    check fails(5.kg > 10)

  test "Comparisons: `<=` for units of same type":
    let x = 5.kg
    let y = 10.kg
    let z = 10.kg
    check x <= y == true
    check z <= y == true
    check fails(5.kg <= 10)

  test "Comparisons: `>=` for units of same type":
    let x = 5.kg
    let y = 10.kg
    let z = 10.kg
    check x >= y == false
    check z >= y == true
    check fails(5.kg >= 10)

  test "Comparisons: `==` for units of same type":
    let x = 5.kg
    let y = 10.kg
    let z = 10.kg
    check x == y == false
    check z == y == true
    check fails(5.kg == 10)

  test "Comparisons: `!=` for units of same type":
    let x = 5.kg
    let y = 10.kg
    let z = 10.kg
    check x != y == true
    check z != y == false
    check fails(5.kg != 10)

  test "Comparisons: `<` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.g
    check x < y == true

  test "Comparisons: `>` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.g
    check x > y == false

  test "Comparisons: `<=` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.g
    let z = 10.kg
    check x <= y == true
    check z <= y == true

  test "Comparisons: `>=` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.g
    let z = 10.kg
    check x >= y == false
    check z >= y == true

  test "Comparisons: `==` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.kg
    let z = 10.kg
    check x == y == false
    ## TODO: broken due to internal `==` of floating point numbers. Need `almostEqual`?
    # check z == y == true

  test "Comparisons: `!=` for units of different type of same quantity":
    let x = 5.kg
    let y = 10_000.kg
    let z = 10.kg
    check x != y == true
    ## TODO: broken due to internal `==` of floating point numbers. Need `almostEqual`?
    # check z != y == false

#Suite "Unchained - other types do not match macros":
  # how do we check this?

suite "Unchained - Units and procedures":
  test "Defining a function taking units and returning units":
    let a = 9.81.m•s⁻²
    let b = 4.2
    let c = b.m•s⁻²
    let mass = 12.kg

    proc force(m: KiloGram, a: Meter•Second⁻²): Newton =
      result = m * a

    check typeof(force(mass, a)) is Newton
    check force(mass, a) == 117.72.Newton
    check fails(force(a, mass))

  test "Functions disallow wrong SI unit arguments":
    proc E_to_γ(E: GeV): UnitLess =
      result = E.to(Joule) / (m_μ * c * c) + 1
    check E_to_γ(1.GeV) =~= 10.46446502980806.UnitLess
    var res = 10.eV
    res = res - 5.eV
    check fails(E_to_γ(res))
    ## NOTE: we cannot use `fails` to check if this does not compile, because
    ## `compiles` seems to only expand the `first` macro. But the expanded code
    ## is the one that actually causes the CT error...
    check fails(5.eV.E_to_γ)
    check fails(5.eV.E_to_γ())
    check 1.GeV.E_to_γ() =~= 10.46446502980806.UnitLess
    check 1.GeV.E_to_γ =~= 10.46446502980806.UnitLess
    check 9.mol•mol⁻¹ == 9.UnitLess
    ## the following would work if `sqrt` was lifted to units of course!
    check fails(9.eV.sqrt)
    check fails(9.eV.sqrt.ln.sqrt)

suite "Unchained - Conversion between units":
  test "Converting different SI prefixes":
    let a = 10.kg
    check a.to(g) == 10_000.Gram
    check a.to(ng) == 10e12.NanoGram

    let f = 1.N
    check f.to(kN) == 0.001.KiloNewton
    check f.to(MN) == 1e-6.MegaNewton

  test "Converting different SI prefixes in product of units":
    ## TODO: allow `to` to generate new units!
    defUnit(kg•m⁻²)
    let a = 10.g•m⁻²
    check a.to(kg•m⁻²) == 0.01.KiloGram•Meter⁻²

suite "Unchained - CT errors":
  test "Error on regular digit as exponent":
    doAssert fails(10.kg•m⁻2) # invalid `2` instead of `²`

  test "Converting fails for wrong powers":
    let a = 10.m²
    check fails(a.to(mm))

  test "Converting fails for wrong quantities":
    let a = 10.m²
    check fails(a.to(kg))

  test "Converting different SI prefixes including power != 1":
    defUnit(m²)
    defUnit(mm²)
    defUnit(m⁻²)
    defUnit(mm⁻²)
    let a = 10.m²
    check a.to(mm²) == 10_000_000.MilliMeter²

    let b = 10.m⁻²
    ## TODO: FIXME comparison fails due to ???. Values are correct though!
    #check b.to(mm⁻²) == 10e-5.MilliMeter⁻²

suite "Unchained - Test of individual units":
  test "Tesla":
    let B = 10.GT
    check typeof(B) is GigaTesla # `exclude` in SI generation was broken
    check B * 10.s == 100_000_000_000.T•s # math works
    check B / 10.s == 1_000_000_000.T•s⁻¹
    check B + 1.T  == 10_000_000_001.T
    check B - 1.T  ==  9_999_999_999.T

  test "Becquerel":
    let A = 10.GBq
    check A.to(Bq) == 10_000_000_000.Bq
    check typeof(A) is GigaBecquerel
    proc counts(A: Bq, time: Second): float =
      # counts in a given time
      result = A * time
    check counts(A.to(Bq), 10.s) == 100_000_000_000.0
    # auto conversion of Giga works + math works
    check A * 10.s == 100_000_000_000.0
    check A / 10.s == 1_000_000_000.s⁻²
    check A + 1.Bq == 10_000_000_001.Bq
    check A - 1.Bq ==  9_999_999_999.Bq

suite "Unchained - Conversion between units requiring scale (no SI prefix)":
  test "Conversion of eV to Joule":
    let x = 1.eV
    # use `e` as `float` and manually convert `Joule` to guarantee same number
    check x.to(Joule) == e.float.Joule

    let y = 10.kg * 9.81.m•s⁻² * 10.m # Potential energy of 10 kg on Earth in 10m height
    check y == (10 * 9.81 * 10).Joule
    check y.to(eV) =~= (981.0 / e.float).eV

    defUnit(J•m⁻¹)
    defUnit(eV•m⁻¹)
    defUnit(MeV•m⁻¹)

    let z = 10.kg * 9.81.m•s⁻²
    check z.to(J•m⁻¹) =~= 98.1.J•m⁻¹
    check z.to(eV•m⁻¹) =~= (98.1 / e.float).eV•m⁻¹
    check z.to(MeV•m⁻¹) =~= (98.1 / e.float / 1e6).MeV•m⁻¹

  test "Conversion of Joule to eV":
    # use `e` as `float` and manually convert `Joule` to guarantee same number
    let x = e.float.J
    check x.to(eV) == 1.eV

  test "Conversion between degrees and radian":
    let x = 180.Degree
    check typeof(x.to(Radian)) is Radian
    check typeof(x.to(Radian).to(Degree)) is Degree
    check x.to(Radian) =~= Pi.Radian
    check x.to(Radian).to(Degree) =~= x

suite "Unchained - Type definitions":
  test "Automatic type definitions: `.` operator defines not existing units":
    ## have to trust me m⁶ is not pre defined :P
    check fails(Meter⁶ is Meter⁶) # compare with itself to have meaningful statement
    let x = 10.m⁶
    # now works
    check Meter⁶ is Meter⁶

  test "Automatic type definitions: Non existing compound types are defined and can be used":
    ## have to trust me m⁶ is not pre defined :P
    let a = 10.m * 10.m * 10.m * 10.m * 10.m * 10.m
    check typeof(a) is Meter⁶
    check a == 1e6.Meter⁶

    # can now be used
    let b = 10.m⁶
    check typeof(b) is Meter⁶
    check b == 10.m⁶

  test "Manual type definitons: using `defUnit`":
    defUnit(Meter•Second⁻⁷)
    proc foo(x: Meter•Second⁻⁷): Meter•Second⁻⁷ = result = 2 * x

    let a = 10.Meter•Second⁻⁷
    check foo(a) == 20.Meter•Second⁻⁷

  test "Unit definition defines both shorthand and long hand":
    ## TODO :fix
    defUnit(kg•m•J•F)
    when false:
      let a = 10.kg•m•J•F
      let b = 10.KiloGram•Meter•Joule•Farad

suite "Unchained - isAUnit concept checking":
  test "Matches units correctly":
    check isAUnit(Meter)
    check isAUnit(Newton)
    defUnit(m•m•m•m•m•m)
    check isAUnit(m•m•m•m•m•m)
    defUnit(KiloGram•Meter•Joule)
    check isAUnit(KiloGram•Meter•Joule)
    check not isAUnit(float)
    check not isAUnit(string)
    check not isAUnit(seq[string])

suite "Unchained - syntax in accented quotes":
  test "Basic units in accented quotes":
    check 5.`Meter*Second^-1` == 5.m•s⁻¹
    check 5.`m*s^-1` == 5.m•s⁻¹
    check 10.`Mol^-1*KiloGram^2` == 10.mol⁻¹•kg²

    defUnit(`kg*m*s^-2`)
    proc test(f: `kg*m*s^-2`): kg =
      result = f / 9.81.m•s⁻²
    check test(98.1.kg•m•s⁻²) =~= 10.kg
    check test(98.1.`kg*m*s^-2`) =~= 10.kg

suite "Unchained - practical examples turned tests":
  test "Vacuum pumping time":
    defUnit(Pa•m³•s⁻¹•m⁻²)
    defUnit(Pa•m³•s⁻¹)
    defUnit(Pa•m³)
    defUnit(m²)
    defUnit(L•s⁻¹)
    defUnit(m³•s⁻¹)

    proc vacTime(q_des: Pa•m³•s⁻¹•m⁻², A: m², t: s, S_HV: L•s⁻¹, p_3: mbar): Hour =
      result = (q_des * A * t / (4 * p_3.to(Pa) * S_HV.to(m³•s⁻¹))).to(Hour)

    check vacTime((2.7e-4).Pa•m³•s⁻¹•m⁻², 35.311.m², 3600.s, 685.L•s⁻¹, 1.0e-7.mbar) =~= 347.9551.Hour

  test "Cosine with radian argument":
    let ω = 100.rad•s⁻¹
    let A = 10.cm
    let φ = Pi.rad
    let t = 1.s
    let argument = ω * t + φ
    check typeof(argument) is Radian
    check typeof(ω) is Second⁻¹•Radian
    check typeof(A * cos(argument)) is CentiMeter
    check A * cos(argument) =~= -8.62319.cm

  test "Density of argon":
    defUnit(g•cm⁻³)
    defUnit(g•mol⁻¹)
    proc density(p: mbar, M: g•mol⁻¹, temp: Kelvin): g•cm⁻³ =
      ## returns the density of the gas for the given pressure.
      ## The pressure is assumed in `mbar` and the temperature (in `K`).
      ## Returns the density in `g / cm^3`
      let gasConstant = 8.314.J•K⁻¹•mol⁻¹ # joule K^-1 mol^-1
      let pressure = p.to(Pa) # pressure in Pa (not necessarily needed to be done manually)
      # convert to `g•cm⁻³` as desired
      result = (pressure * M / (gasConstant * temp)).to(g•cm⁻³)
    # Argon density at 20°C at 1050 mbar
    let M_Ar = 39.95.g•mol⁻¹ # molar mass. Numerically same as relative atomic mass
    let ρAr = density(1050.mbar, M_Ar, temp = 293.15.K)
    check ρAr =~= 0.0017211.g•cm⁻³
    defUnit(g•L⁻¹)
    check ρAr.to(g•L⁻¹) =~= 1.7211.g•L⁻¹

  test "Ionization energy of argon":
    proc I[T](z: int): T =
      result = (10.eV * z.float).to(T) # 188.0 eV from NIST table for Ar (Z = 18)
    check typeof(I[eV](18)) is eV
    check I[eV](18) =~= 180.eV

    let x: eV = 10.eV * 18.0
    check typeof(x) is eV
    check x =~= 180.eV

    check typeof(I[Joule](18)) is Joule
    let e = 1.602176634e-19
    check I[Joule](18) =~= (180.0 * e).J
    check I[Joule](18) =~= 180.eV.to(Joule)

  test "Product of base units units with keV do not convert":
    let time = 3318.Hour
    let weight = (time.to(Second) * 0.5.cm * 0.5.cm * 0.2.keV)
    defUnit(keV•cm²•s)
    check typeof(weight) is keV•cm²•s
    check weight =~= (3318.0 * 3600.0 * 0.5 * 0.5 * 0.2).keV•cm²•s

  test "Inverse time multiplied with time yields UnitLess":
    let time = 3318.Second⁻¹
    let weight = time * 500.Second
    # this checks that the result is *not* Second•Hertz, which while valid is
    # not our desired unit for this. It's a check for he `needConversion` logic
    check typeof(weight) is UnitLess
    check weight =~= (3318.0 * 500.0).UnitLess

suite "Unchained - imperial units":
  test "Pound":
    block:
      let x = 1.lbs
      let y = 1.kg
      check type(x + y) is KiloGram
      check x + y =~= 1.45359237.KiloGram
    block:
      let x = 1.Pound
      let y = 1.KiloGram
      check type(x + y) is KiloGram
      check x + y =~= 1.45359237.KiloGram

  test "Inch":
    block:
      let x = 1.inch
      let y = 1.m
      check type(x + y) is Meter
      check x + y =~= 1.0254.Meter
    block:
      let x = 1.Inch
      let y = 1.Meter
      check type(x + y) is Meter
      check x + y =~= 1.0254.Meter

  test "Foot":
    block:
      let x = 1.ft
      let y = 1.m
      check type(x + y) is Meter
      check x + y =~= 1.3048.Meter
    block:
      let x = 1.Foot
      let y = 1.Meter
      check type(x + y) is Meter
      check x + y =~= 1.3048.Meter
    block Multiply:
      let x = 5.m
      let y = 3.ft⁻¹
      check x * y =~= 49.2126.UnitLess
      check typeof(x * y) is UnitLess
    block Divide:
      let x = 15.m
      let y = 5.m.to(ft)
      check typeof(y) is Foot
      check x / y =~= 3.UnitLess
      check typeof(x / y) is UnitLess


  test "Yard":
    block:
      let x = 1.yd
      let y = 1.m
      check type(x + y) is Meter
      check x + y =~= 1.9144.Meter
    block:
      let x = 1.Yard
      let y = 1.Meter
      check type(x + y) is Meter
      check x + y =~= 1.9144.Meter

  test "Ounce":
    block:
      let x = 1.oz
      let y = 1.kg
      check type(x + y) is KiloGram
      check x + y =~= 1.028349523.KiloGram
    block:
      let x = 1.Ounce
      let y = 1.KiloGram
      check type(x + y) is KiloGram
      check x + y =~= 1.028349523.KiloGram

  test "Slug":
    block:
      let x = 1.slug
      let y = 1.kg
      check type(x + y) is KiloGram
      check x + y =~= 15.593903.KiloGram
    block:
      let x = 1.Slug
      let y = 1.KiloGram
      check type(x + y) is KiloGram
      check x + y =~= 15.593903.KiloGram

  #test "Acre":
  #  block:
  #    let x = 1.acre
  #    let y = 1.m²
  #    check type(x + y) is Meter²
  #    check x + y =~= 4047.8564.Meter²
  #  block:
  #    let x = 1.Acre
  #    let y = 1.Meter²
  #    check type(x + y) is Meter²
  #    check x + y =~= 4047.8564.Meter²

  test "Pound-force":
    block:
      let x = 1.lbf
      let y = 1.N
      check type(x + y) is Newton
      check x + y =~= 5.4482216.Newton
    block:
      let x = 1.PoundForce
      let y = 1.Newton
      check type(x + y) is Newton
      check x + y =~= 5.4482216.Newton

    block:
      # bug found by @hugogranstrom
      defUnit(N•s)
      defUnit(lbf•s)

      let lbfs = 1.lbf•s
      let Ns = lbfs.to(N•s)

      check Ns =~= 4.44822.N•s
      check type(Ns) is N•s


import std/sugar
suite "Unchained - Bug issues":
  test "Different names in equality operator":
    block:
      let a = 1.N•s
      let b = 1.`N*s`
      check typeof(a) is typeof(b)
      check a == b
    block:
      let a = 1.N•s
      let b = 1.kg•m•s⁻¹
      # this cannot hold, as the nim compile does not recognize that they are the same
      check typeof(a) isnot typeof(b)
      # but this works
      check a == b

  test "Division of compound units (issue #16)":
    proc xrayEnergyToFreq(E: keV): Hz =
      result = E.to(Joule) / hp
    check 1.keV.xrayEnergyToFreq =~= 2.41799e17.Hz

  test "Math with unitful `const` variables works":
    ## this was previously broken, issue #5. The unit was dropped when parsing
    ## the unit, as the `isUnitLessNumber` check returned `true` for `const` values
    const g_aγ = 1e-10.GeV⁻¹
    let x = g_aγ * 1.0.eV²
    check typeof(x) is ElectronVolt
    check x =~= 1e-19.eV

  test "CT error due to sequence arguments of units":
    # reported by Arkanoid on matrix/discord
    func `*` [§L, §R](a: openArray[§L], b: §R): auto =
      collect(newSeqOfCap(a.len)):
        for i in 0..<a.len:
          a[i] * b
    let
      a = @[1.UnitLess, 2.UnitLess, 3.UnitLess]
      b = 5.m•s⁻¹
    check a * b == @[5.m•s⁻¹, 10.m•s⁻¹, 15.m•s⁻¹]

suite "Utils":
  test "Power w/ static integer exponents for floats":
    let x = 5
    check x ^ 0  == 1.0
    check x ^ 1  == x
    check x ^ 2  == x * x
    check x ^ 3  == x * x * x
    check x ^ 4  == x * x * x * x
    check x ^ 5  == x * x * x * x * x
    check x ^ -1 == 1 / x
    check x ^ -2 == 1 / (x * x)
    check x ^ -3 == 1 / (x * x * x)
    check x ^ -4 == 1 / (x * x * x * x)
    check x ^ -5 == 1 / (x * x * x * x * x)

  test "Power w/ static integer exponents for units":
    let x = 5.kg
    check x ^ 0  == 1.0
    check x ^ 1  == x
    check x ^ 2  == x * x
    check x ^ 3  == x * x * x
    check x ^ 4  == x * x * x * x
    check x ^ 5  == x * x * x * x * x
    check x ^ -1 == 1 / x
    check x ^ -2 == 1 / (x * x)
    check x ^ -3 == 1 / (x * x * x)
    check x ^ -4 == 1 / (x * x * x * x)
    check x ^ -5 == 1 / (x * x * x * x * x)


#converter to_eV(x: GeV): eV =
#  echo "toEv!"
#  (x.float * 1e-9).eV

#proc E_to_γ(E: eV | GeV | Joule): UnitLess =
#  result = E.to(Joule) / (m_μ * c * c) + 1
#
#let muE = 1.0.GeV #e.GeV # eV
#let muγ = E_to_γ(muE)
#echo muγ
#
#let gravity = 1.0.kg * 9.81.m•s⁻²
#echo gravity








#block:
#  # product of prefixed SI unit keeps same prefix unless multiple units of same quantity involved
#  let a = 1.m•s⁻²
#  let b = 500.g
#  check typeof(a * b) is Gram•Meter•Second⁻²
#  #check typeof(a * b) is MilliNewton
#  check a * b == 500.g•m•s⁻²
#block:
#  ## different order produces same units
#  let mass = 5.kg
#  let a = 9.81.m•s⁻²
#  # unit multiplication has to be commutative
#  let F: Newton = mass * a
#  let F2: Newton = a * mass # TODO
#  # unit division works as expected
#  check typeof(F / mass) is Meter•Second⁻²
#  check F / mass == a
#block:
# # pre-defined physical constants
# let E_e⁻_rest: Joule = m_e * c*c # math operations `*cannot*` use superscripts!
# # m_e = electron mass in kg
# # c = speed of light in vacuum in m/s
#block:
#  # automatic CT error if argument of e.g. sin, ln are not unit less
#  let x = 5.kg
#  let y = 10.kg
#  discard sin(x / y) ## compiles gives correct result (~0.48)
#  let x2 = 10.m
#  # sin(x2 / y) ## errors at CT due to non unit less argument
#block:
#  # imperial units
#  let mass = 100.lbs
#block:
#  # mixing of non SI and SI units (via conversion to SI units)
#  let m1 = 100.lbs
#  let m2 = 10.kg
#  # check typeof(m1 + m2) is KiloGram ## TODO: fix taking order on addition etc
#  ## equal, but check is broken
#  check m1.to(kg) + m2 == 55.36.KiloGram


#block:
#  # units using english language (using accented quotes)
#  let a = 10.`meter per second squared`
#  let b = 5.`kilogram meter per second squared`
#  check typeof(a) is Meter•Second⁻²
#  check typeof(b) is Newton
#  check a == 10.m•s⁻²
#  check b == 5.N

## Yet to be implemented
#block:
#  # natural unit support (c = 1, h = 1)
#  let speed: NaturalVelocity = 0.1.UnitLess # fraction of c
#  let m_e: NaturalMass = 511.keV
#  # math between natural units remains natural
#  let p: NaturalMomentum = speed * m_e
#  check p == 51.1.keV
#block:
#  # auto conversion of natural units
#  let a = 10.MeV
#  let b = 200.eV
#  check typeof(a / b) is UnitLess # `UnitLess` is
#  check a / b == 50_000.Unitless
