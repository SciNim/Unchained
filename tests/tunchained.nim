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

proc `=~=`(a, b: SomeUnit|UnitLess): bool =
  result = almostEq(a.float, b.float, epsilon = 1e-5) and type(a) is type(b)

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

  test "Math of compound units":
    ## TODO: for certain compound units math is somewhat broken, as
    ## we don't transform to the correct units.
    ## Consider
    let x = 1.Liter
    let y = 1.m³
    ## TODO: fix me. Should be m³!
    check type(x + y) is Liter
    check type(y + x) is Meter³
    echo "I'm a *WRONG* test illustrating issue #9. Fix me!"
    check x + y =~= 1.001.Liter ## WRONG!!!
    check y + x =~= 1.001.Meter³ ## correct

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
    defUnit(Kg•Meter•Joule)
    check isAUnit(Kg•Meter•Joule)
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
#  # check typeof(m1 + m2) is Kg ## TODO: fix taking order on addition etc
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
