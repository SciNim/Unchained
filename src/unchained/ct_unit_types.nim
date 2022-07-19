import core_types, quantities

type
  DefinedUnitType* = enum
    utBase, utDerived
  #CompoundType = enum # ???
  #  ctBase, ctCompound
  #CTUnitValue* = object
  #  unit*: DefinedUnit
  #  siPrefix*: SIPrefix
  #  value*: float
  ## Turn this into a `DefinedUnit`?
  ## Then have a `CTUnit` that is a *specific instance* of a unit with
  ## possibly different exponents, prefixes etc? similar to currently used CTUnit

  ## NOTE: maybe split them up by BaseUnit and DefinedUnit after all instead of having
  ## a variant type like this?

  DefinedUnit* = ref object
    name*: string
    basePrefix*: SiPrefix ## This prefix defines the possible prefix of the *base unit*, which
                          ## in SI is only relevant for KiloGram
    short*: string
    quantity*: CTQuantity
    case kind*: DefinedUnitType
    of utBase: discard
    of utDerived: conversion*: UnitProduct
    # case baseTyp: BaseType
    # case compTyp: CompoundType
    ## Need one field to define if it's a base unit or not
    ## Need another to define if it's a simple unit or a compound?
    ## Need another field to define conversion to base units?
    ##
    ## Well: the quantity tells us whether it's a base or a compound unit.
    ## And whether it's a *base unit* we know based on whether it has a conversion
    ## associated to it.

  UnitInstance* = ref object
    name*: string # the part of the nim node that was parsed into this
    unit*: DefinedUnit ## The actual underlying unit
    prefix*: SiPrefix # possibly differentiating prefix
    power*: int
    value*: float

  UnitProduct* = ref object # a product of multiple (possibly compound) units
    value*: float # value of this unit poduct
    units*: seq[UnitInstance]
    #siPrefix*: float # as a pure float value

proc newUnitInstance*(name: string,
                      u: DefinedUnit,
                      power: int,
                      prefix: SiPrefix,
                      value: float = 1.0): UnitInstance =
  result = UnitInstance(name: name,
                        unit: u,
                        power: power,
                        prefix: prefix,
                        value: value)

proc `==`*(a, b: DefinedUnit): bool =
  result = a.name == b.name

proc `==`*(a, b: UnitInstance): bool =
  result = (a.unit == b.unit and a.prefix == b.prefix and a.power == b.power)
