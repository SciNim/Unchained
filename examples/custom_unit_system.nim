## A *really* stupid example of how to define quanties & units
## See also `cgs_unit_system.nim` for a more realistic use case of defining the
## cgs units as the base units (instead of SI). This example is to showcase that
## you can define whatever you want as quantities & units on top.

## Import the CT API to generate quantites & units
import unchained / api # <- API defining operations on units
import unchained / ct_api # <- compile time API to declare quantities and units
import unchained / define_units

declareQuantities:
  Base:
    Line
    Triangle
    Rectangle
    Octagon
    Circle
  Derived:
    Sign: [(Line, 1), (Octagon, 1)] # a stick with an octagon on top
    Car: [(Circle, 4), (Rectangle, 1)] # clearly a car. 4 tires and a chassis

## If you wish to put this unit system into a separate file, you need to export the `commonQuantity`
## from the `define_units` submodule!
#export define_units.commonQuantity

## Generate all base types (base units representing the defined quantities) and
## possible other (compound) types that are defined via a conversion to an existing
## base type.
declareUnits:
  BaseUnits: # SI base units
    Stick:
      short: s
      quantity: Line
    Triard:
      short: t
      quantity: Triangle
    Paper:
      short: p
      quantity: Rectangle
    Octard:
      short: o
      quantity: Octagon
    Pi:
      short: pi
      quantity: Circle
  Derived:
    RaceCar:
      short: rc
      quantity: Car
    Truck:
      short: trk
      quantity: Car
      conversion: 0.1.RaceCar # hey, trucks are slower, so they surely are less car, no?
    StopSign:
      short: ss
      quantity: Sign


let x = 1.RaceCar
echo x, " in trucks ? ", x.to(Truck), ". Maybe this means we need 10 trucks for one race car?"

let lin = 1.Stick
let oct = 1.Octard
echo lin, " times ", oct, " is ", lin * oct, ". Well, I guess?"

let pi2 = 1.pi * 1.pi
echo "π² = ", pi2

proc funRace(numRaceCars: RaceCar, tracks: Pi) =
  echo "We have ", numRaceCars, " running on these ", tracks

funRace(5.rc, 3.pi) # eehhh, yeah

# dividing a StopSign by an Octard leaves you with a stick!
echo 1.StopSign, " / ", 1.Octard, " = ", (1.StopSign / 1.Octard).to(Stick) # if Octard⁻¹•StopSign was not equivalent the
                                                                           # conversion would fail!

## I think you get the point. It works.
