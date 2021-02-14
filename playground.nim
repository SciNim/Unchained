import unchained, math

template fails(x: untyped): untyped =
  when compiles(x):
    false
  else:
    true

block:
  let a = 9.81.m•s⁻²
  let b = 4.2
  let c = b.m•s⁻²
  let mass = 12.kg

  #static: echo "1"
  proc force(m: KiloGram, a: Meter•Second⁻²): Newton =
    #static: echo "iaeiae"
    #static: echo "2"
    result = m * a

  #static: echo "2"

  let f = force(mass, a) # works
  echo f {.explain.}
  #static: echo "iae"
  echo force(mass, a) {.explain.} # works
  #static: echo "more"
  #echo F(a, m) # CT error


  #echo (f * f).to(kN²)
  echo f.to(N)
  echo f.to(kN)
  echo f.to(MN)
  type m⁻¹ = distinct Meter
  echo 5.m * 5.m⁻¹



#static: echo "fails"
doAssert fails(a + mass)
#static: echo "ok"
doAssert fails(a - mass)
echo "failed"

let small = 10.g
echo small
let big = 1.kg
echo small + big

block:
  let expl = 5.N
  let impl = 10.kg•m•s⁻²
  echo "Explicit plus implicit ", expl + impl
block:
  let expl = 5.N
  let impl = 10.kg•m•s⁻³
  doAssert fails(expl + impl)


echo 5.m•m
echo 5.m•m⁻¹

when false:
  ## the following sort of works, but the symbols of the locally defined type
  ## and the argumnt given when calling the function don't match
  template def(arg: untyped): untyped =
    when not declared(arg):
      type arg = distinct CompoundQuantity
    arg
  proc foo(x: def(Meter•Second⁻⁶)) =
    echo x

defUnit(Meter•Second⁻⁷)
proc foo2(x: Meter•Second⁻⁷) =
  echo x

foo2(5.m•s⁻⁷)


#let a0 = 10.`meter per second squared`
#echo a0
##echo a0 + 10.m•s⁻²
##
proc testme() =
  let nonExist = 10.m * 10.m * 10.m * 10.m * 10.m * 10.m
  echo nonExist
  echo type(nonExist)

  let nowExists: Meter⁶ = 10.m•m•m•m•m•m + nonExist
  echo nowExists.repr
  let what = block:
               type WowDoge = distinct float
               5.5.WowDoge
  echo type(what)
testme()
#

#
#
static:
  discard isAUnit(Meter)
  discard isAUnit(Newton)
  #discard isAUnit(m•m•m•m•m•m)
  #discard isAUnit(m•s⁻¹)
  #discard isAUnit(Kg•Meter•Joule)


#echo 5.5.WowDoge
#import unittest
#suite "Units":
#  test "Adding units of different quantities is an error":
#
#    fails:
#      let y = 2.meter + 4.kg # error

#echo type(x / Second)

#converter toFloat(j: Joule): float = j.float
#converter toJoule(x: eV): Joule =
#  echo "toJoule"
#  (x.float * e).Joule
#converter to_eV(x: GeV): eV =
#  echo "toEv!"
#  (x.float * 1e-9).eV

proc E_to_γ(E: eV | GeV | Joule): UnitLess =
  result = E.to(Joule) / (m_μ * c * c) + 1

let muE = 1.0.GeV #e.GeV # eV
let muγ = E_to_γ(muE)
echo muγ

let gravity = 1.0.kg * 9.81.m•s⁻²
echo gravity








echo me.type
echo me




import unittest
# following already work unless `TODO` (not all units implemented)
block:
  # defining simple units
  let mass = 5.kg
  let a = 9.81.m•s⁻²
block:
  # addition and subtraction of same units
  let a = 5.kg
  let b = 10.kg
  check typeof(a + b) is Kg
  check a + b == 15.kg
  check typeof(a - b) is Kg
  check a - b == (-5).kg
block:
  # addition and subtraction of units of the same ``quantity`` but different scale
  let a = 5.kg
  let b = 500.g
  check typeof(a + b) is Kg
  check a + b == 5.5.kg
  # if units do not match, the SI unit is used!
block:
  # product of prefixed SI unit keeps same prefix unless multiple units of same quantity involved
  let a = 1.m•s⁻²
  let b = 500.g
  check typeof(a * b) is Gram•Meter•Second⁻²
  #check typeof(a * b) is MilliNewton
  check a * b == 500.g•m•s⁻²
block:
  let mass = 5.kg
  let a = 9.81.m•s⁻²
  # unit multiplication has to be commutative
  let F: Newton = mass * a
  let F2: Newton = a * mass # TODO
  # unit division works as expected
  check typeof(F / mass) is Meter•Second⁻²
  check F / mass == a
block:
  # conversion between units of the same quantity
  let f = 10.N
  check typeof(f.to(kN)) is KiloNewton
  check f.to(kN) == 0.01.kN
block:
 # pre-defined physical constants
 let E_e⁻_rest: Joule = m_e * c*c # math operations `*cannot*` use superscripts!
 # m_e = electron mass in kg
 # c = speed of light in vacuum in m/s
block:
  # automatic CT error if argument of e.g. sin, ln are not unit less
  let x = 5.kg
  let y = 10.kg
  discard sin(x / y) ## compiles gives correct result (~0.48)
  let x2 = 10.m
  # sin(x2 / y) ## errors at CT due to non unit less argument
block:
  # imperial units
  let mass = 100.lbs
block:
  # mixing of non SI and SI units (via conversion to SI units)
  let m1 = 100.lbs
  let m2 = 10.kg
  # check typeof(m1 + m2) is Kg ## TODO: fix taking order on addition etc
  ## equal, but check is broken
  check m1.to(kg) + m2 == 55.36.KiloGram


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
