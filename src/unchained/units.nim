import math, macros, options, sets, tables, strutils, unicode, typetraits, strformat

## This file is the main user facing API
import core_types, ct_unit_types, macro_utils, define_units, quantities

proc pretty(x: UnitInstance, short = false): string = x.toNimType(short)
proc pretty(x: UnitProduct, short = false): string = x.toNimTypeStr(short)

macro quantityList*(): untyped =
  result = nnkBracket.newTree()
  result.add newLit"Quantity"
  result.add newLit"CompoundQuantity"
  result.add newLit"Unit"
  result.add newLit"Quantity"
  result.add newLit"SiUnit"
  result.add newLit"DerivedSiUnits"
  result.add newLit"SomeQuantity"
  result.add newLit"DerivedQuantity"
  result.add newLit"BaseQuantity"

macro isAUnit*(x: typed): untyped =
  ## NOTE: it's really hard to replace this by something cleaner :/
  ## Ideally this should be replaced by something that uses shared logic with
  ## `getUnitTypeImpl` & making use of CT tables (possibly of objects?)
  let x = x.resolveAlias()
  case x.kind
  of nnkSym, nnkDistinctTy:
    let typ = x
    var xT = if typ.kind == nnkDistinctTy: typ[0] else: typ
    while xT.strVal notin quantityList():
      xT = xT.getTypeImpl
      case xT.kind
      of nnkDistinctTy:
        xT = xT[0]
      else:
        return newLit false
  else:
    return newLit false
  ## in this case investigation is true
  result = newLit true

## The main concept used for type matching of units
type
  SomeUnit* = concept x
    isAUnit(x)

## CTCompoundUnit logic

proc sanitizeInput(n: NimNode): NimNode =
  # remove all `nnkConv, nnkHiddenStdConv and nnkStmtListExpr`
  let tree = n
  proc sanitize(n: NimNode): NimNode =
    if n.len == 0:
      #result = n
      case n.kind
      of nnkSym: result = ident(n.strVal)
      else: result = n
    else:
      case n.kind
      of nnkConv, nnkHiddenStdConv, nnkHiddenCallConv: result = n[1].sanitize
      of nnkStmtListExpr: result = n[1].sanitize
      else:
        result = newTree(n.kind)
        for ch in n:
          result.add sanitize(ch)
  ## NOTE: sanitation like this is much more problematic than I thought
  ## We end up with many edge cases, which suddenly either:
  ## - produce recursive loops
  ## - cause weird CT errors as we somehow manage to strip too much information
  ## Disabled for now.
  result = tree#.sanitize()

## General user facing API

## This is a *forced* conversion of a unit to a float. It simply removes any unit from
## the type. Currently just `x.float`. Might change in the future.
proc toFloat*[T: SomeUnit](x: T): float = x.float

## auto conversion of `UnitLess` to `float` is possible so that e.g. `sin(5.kg / 10.kg)` works as expected!
converter toRawFloat*(x: UnitLess): float = x.float
converter toUnitLess*(x: SomeNumber): UnitLess = x.UnitLess
converter toUnitLess*(x: float64): UnitLess = x.UnitLess

import hashes
proc hash*[T: SomeUnit](x: T): Hash =
  result = result !& hash($typeof(T))
  result = result !& hash(x.float)
  result = !$result

## Pretty printing of units

macro shortName(t: typed): untyped =
  let typ = t.parseDefinedUnit()
  result = newLit typ.pretty(short = true)

macro unitName(t: typed): untyped =
  let typ = t.parseDefinedUnit()
  result = newLit typ.pretty(short = false)

proc pretty*[T: SomeUnit](s: T, precision: int, short: bool): string =
  result = s.float.formatFloat(precision = precision)
  result.trimZeros()
  if not short:
    let typStr = unitName(T)
    when not defined(noUnicode):
      result.add &" {typStr}"
    else:
      result.add &" {typStr}"
  else:
    let typStr = shortName(T)
    when not defined(noUnicode):
      result.add &" {typStr}"
    else:
      result.add &" {typStr}"

proc `$`*[T: SomeUnit](s: T): string = pretty(s, precision = -1, short = false)

macro defUnit*(arg: untyped, toExport: bool = false): untyped =
  ## Helper template to define new units (not required to be used manually)

  ## TODO: emit both short and long hand version of the given unit
  let argCT = parseDefinedUnit(arg)
  let toExport = toExport.strVal == "true"
  ## TODO: instead of just using the long version, what to do for
  ## J•m or something like this? For max compatibility the RHS
  ## should actually be the base unit stuff.
  if true: # when would we want the other branch now?
    let resType = argCT.simplify(mergePrefixes = true).toNimType()
    if resType.strVal != "UnitLess":
      if not toExport:
        result = quote do:
          when not declared(`resType`):
            type `resType` = distinct CompoundQuantity
          when not declared(`arg`):
            type `arg` = `resType`
      else:
        result = quote do:
          when not declared(`resType`):
            type `resType`* = distinct CompoundQuantity
          when not declared(`arg`):
            type `arg`* = `resType`
    else:
      if not toExport:
        result = quote do:
          when not declared(`arg`):
            type `arg` = `resType`
      else:
        result = quote do:
          when not declared(`arg`):
            type `arg`* = `resType`
  else:
    if arg.strVal != "UnitLess":
      if not toExport:
        result = quote do:
          when not declared(`arg`):
            type `arg` = distinct CompoundQuantity
      else:
        result = quote do:
          when not declared(`arg`):
            type `arg`* = distinct CompoundQuantity

## TODO: we should really combine these macros somewhat?
macro `==`*[T: SomeUnit; U: SomeUnit](x: T, y: U): bool =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float == `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    let needConversion = needConversionToBase(xCT, yCT)
    xCT = xCT.toBaseType(needConversion).simplify()
    yCT = yCT.toBaseType(needConversion).simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    ## TODO: use almostEqual?
    result = quote do:
      (`x`.float * `xScale` == `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float < `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    let needConversion = needConversionToBase(xCT, yCT)
    xCT = xCT.toBaseType(needConversion).simplify()
    yCT = yCT.toBaseType(needConversion).simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    result = quote do:
      (`x`.float * `xScale` < `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<=`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.float <= `y`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    let needConversion = needConversionToBase(xCT, yCT)
    xCT = xCT.toBaseType(needConversion).simplify()
    yCT = yCT.toBaseType(needConversion).simplify()
    let resType = xCT.toNimType()
    # compare scaled to base type units
    result = quote do:
      (`x`.float * `xScale` <= `y`.float * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `+`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if xCT == yCT:
    # excactly the same type, just add
    let resType = xCT.toNimType() # same type, just use `xCT`
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float + `yr`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    var xScale = xCT.toBaseTypeScale()
    var yScale = yCT.toBaseTypeScale()
    # check if conversion (flattening) of a unit is needed
    let needConversion = needConversionToBase(xCT, yCT)
    xCT = xCT.toBaseType(needConversion).simplify()
    yCT = yCT.toBaseType(needConversion).simplify()
    xScale /= xCT.toBaseTypeScale()
    yScale /= yCT.toBaseTypeScale()
    doAssert xCT == yCT, "Conversion to base types failed!"
    let resType = xCT.toNimType()
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float * `xScale` + `yr`.float * `yScale`)
  else:
    error("Different quantities cannot be added! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `-`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()

  if xCT == yCT:
    # excactly the same type, just add
    let resType = xCT.toNimType() # same type, just use `xCT`
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float - `yr`.float)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    var xScale = xCT.toBaseTypeScale()
    var yScale = yCT.toBaseTypeScale()
    # now convert x, y to base types
    let needConversion = needConversionToBase(xCT, yCT)
    xCT = xCT.toBaseType(needConversion).simplify()
    yCT = yCT.toBaseType(needConversion).simplify()
    xScale /= xCT.toBaseTypeScale()
    yScale /= yCT.toBaseTypeScale()

    doAssert xCT == yCT, "Conversion to base types failed!"
    let resType = xCT.toNimType()
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float * `xScale` - `yr`.float * `yScale`)
  else:
    error("Different quantities cannot be subtracted! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

proc `-`*[T: SomeUnit](x: T): T = (-(x.float)).T

proc `+=`*[T: SomeUnit](x: var T, y: T) =
  x = x + y

proc `-=`*[T: SomeUnit](x: var T, y: T) =
  x = x - y

proc `*=`*[T: SomeUnit](x: var T, y: UnitLess) =
  x = x * y

proc `/=`*[T: SomeUnit](x: var T, y: UnitLess) =
  x = x / y

macro `*`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  ## TODO: can we extract the actual mathy part from x, y instead of using the
  ## whole expression? And then reinsert that after our change
  var xCT = parseDefinedUnit(x)
  let yCT = parseDefinedUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()

  # add `yCT` to xCT. Equates a product after simplification
  if xCT == yCT:
    # excactly the same type, just multiply numbers and square unit
    xCT.add yCT
    let resType = xCT.simplify(mergePrefixes = true).toNimType()
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float * `yr`.float)
  else:
    # check if these units need a conversion
    let needConversion = needConversionToBase(xCT, yCT)
    xCT.add yCT
    # define scale before modifying units
    let scaleOriginal = xCT.toBaseTypeScale()
    # and flatten if needed
    xCT = xCT.flatten(needConversion)
    var resTypeCT: UnitProduct
    if needConversion:
      resTypeCT = xCT.toBaseType(needConversion).simplify(mergePrefixes = true)
    else:
      resTypeCT = xCT.simplify(mergePrefixes = true)
    # determine scale of resulting units
    let scaleConv = resTypeCT.toBaseTypeScale() ## WRONG: must not *always* call conversion
    let resType = resTypeCT.simplify().toNimType()
    if scaleOriginal != scaleConv:
      let scale = scaleOriginal / scaleConv
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.float * `yr`.float * `scale`)
    else:
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.float * `yr`.float)

#template `*`*[T: SomeUnit; U: SomeNumber](x: T; y: U{lit}): T = (x.float * y.float).T
#template `*`*[T: SomeUnit; U: SomeNumber](x: U{lit}; y: T): T = (x.float * y.float).T

macro `/`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  let yCT = parseDefinedUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if xCT == yCT:
    # excactly the same type, result is simply unitless
    result = quote do:
      UnitLess(`xr`.float / `yr`.float)
  else:
    # add inverted `yCT` (power -> -power) to xCT. Equates a division after simplification
    # determine if conversion needed
    let needConversion = needConversionToBase(xCT, yCT)
    # perform the "division"
    xCT.add yCT.invert()
    # and determine scale of resulting unit, before flattening / simplification
    let scaleOriginal = xCT.toBaseTypeScale()

    xCT = xCT.flatten(needConversion)
    var resTypeCT: UnitProduct
    if needConversion:
      resTypeCT = xCT.toBaseType(needConversion).simplify(mergePrefixes = true)
    else:
      resTypeCT = xCT.simplify(mergePrefixes = true)
    let scaleConv = resTypeCT.toBaseTypeScale()
    let resType = resTypeCT.simplify().toNimType()
    if scaleOriginal != scaleConv:
      let scale = scaleOriginal / scaleConv
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.float / `yr`.float * `scale`)
    else:
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.float / `yr`.float)

macro sqrt*[T: SomeUnit](t: T): untyped =
  ## Implements the `sqrt` of a given unitful value.
  ##
  ## Fails if the given unit is not a perfect square (i.e. each compound of the full
  ## unit's power is a multiple of 2).
  let typ = t.parseDefinedUnit()

  var mType = typ
  for u in mitems(mType.units):
    if u.power mod 2 == 0: # can be divided
      u.power = u.power div 2
    else:
      error("Cannot take the `sqrt` of input unit " & $(typ.toNimType()) & " as it's not a perfect square!")
  let resType = mType.toNimType()
  let tr = t.sanitizeInput()
  result = quote do:
    defUnit(`resType`)
    `resType`(sqrt(`tr`.float))

proc abs*[T: SomeUnit](t: T): T = (abs(t.float)).T

macro determineScale(x: typedesc, y: typedesc): float =
  ## x and y `have` to be of the same quantity
  # determine which SI prefix ``in total``
  # for that:
  # - get string representation of type
  # - need to split by `•`
  # - walk string and check for SI prefix
  # - if prefix found, set `factor`
  # - if exponent found `⁻` take `factor=factor^power`
  # - multiply `scale` by `factor`
  let xCT = x.parseDefinedUnit()
  let yCT = y.parseDefinedUnit()
  let xScale = xCT.toBaseTypeScale()
  let yScale = yCT.toBaseTypeScale()
  result = newLit(xScale / yScale)

proc to*[T: SomeUnit; U: SomeUnit](x: T, to: typedesc[U]): U =
  ## TODO: replace by macro as well so that we can deal with arbitrary types
  ##
  ## check if conversion possible

  ## TODO: check here if we convert between equivalent quantities, but different
  ## units (`not` just difference in SI). Or rather extend `determineScale`.
  when T is U:
    result = x
  elif commonQuantity(T, U):
    # perform conversion
    ## thus determine scaling factor due to different SI prefixes
    let scale = determineScale(T, U)
    result = (x.float * scale).U
  else:
    {.error: "Cannot convert " & $T & " to " & $U & " as they represent different " &
      "quantities!".}

when false:
  macro toImpl(x: typed, to: static CTCompoundUnit): NimNode =
    ## TODO: replace by macro as well so that we can deal with arbitrary types
    ##
    ## check if conversion possible
    let xCT = parseDefinedUnit(x)
    let yCT = to
    if xCT == yCT:
      result = x
    elif xCT.commonQuantity(yCT):
      # perform conversion
      ## thus determine scaling factor due to different SI prefixes
      let xScale = xCT.toBaseTypeScale()
      let yScale = yCT.toBaseTypeScale()
      let scale = xScale / yScale
      let resType = yCT.toNimType()
      result = quote do:
        defUnit(`resType`)
        `resType`(`x`.float * `scale`)
    else:
      error("Cannot convert " & $(xCT.toNimType()) & " to " & $(yCT.toNimType()) & " as they represent different " &
        "quantities!")

{.experimental: "dotOperators".}
macro `.`*[T: SomeUnit|SomeNumber](x: T; y: untyped): untyped =
  ## macro to allow to generate new types on the fly
  ## TODO: maybe have an explicit distinction between already defined types
  ## and not defined types? Use `.!` or something like this instead?
  let typX = x.getTypeInst()
  let yCT = y.parseDefinedUnit()

  let resType = yCT.simplify(mergePrefixes = true).toNimType()
  # check whether argument is actually `UnitLess` and not something that fails
  # parsing as a unit. TODO: improve unit parsing to handle this? Need a failed
  # parsing state != UnitLess
  let isUnitLess = y.strVal == "UnitLess" or
    (yCT.units.len > 1 and # > 1 means something like mol•mol⁻¹ can work
     resType.strVal == "UnitLess")
  let rewrite = not isUnitLess and resType.strVal == "UnitLess" # parsing failed
  let xr = x.sanitizeInput()
  if not rewrite:
    result = quote do:
      defUnit(`resType`)
      `resType`(`xr`.float)
  else:
    # parsing as a unit failed, rewrite to get possible CT error or correct result
    result = quote do:
      `y` `x`

## Natural unit stuff
proc toNaturalUnitImpl(t: UnitProduct): UnitProduct
proc toNaturalUnitImpl(t: UnitInstance): UnitProduct =
  ## TODO: problem is T is converted into flattened type and mT is kept as milli tesla!!!
  ## TODO2: is this still a problem?
  case t.unit.quantityKind
  of qtFundamental:
    # convert fundamental unit to natural. Note: may still need to be converted to base unit!
    case t.unit.kind
    of utBase:
      # is a base unit, must have `toNaturalUnit`
      doAssert t.unit.quantityKind == qtFundamental
      result = t.unit.toNaturalUnit.clone()
      doAssert result.units.len == 1
      # first modify `value` based on power and prefix of natural unit conversion,
      # i.e. apply the right dimension
      var factor = 1.0
      var uPower = 1
      for u in mitems(result.units):
        doAssert u.prefix == siIdentity
        # adjust power of natural units based on input unit
        factor *= result.value
        #result.value = pow(result.value * u.prefix.toFactor(), u.power.float)
        uPower = u.power
        u.power *= t.power
      # Note: we do *not* multiply in the power of the natural unit, as this is
      # only related to the dimension of the resulting unit, but our conversions are
      # for the correct dimension already
      result.value = pow(factor / t.prefix.toFactor(), t.power.float)
      # now invert the value, as we want it as multiplicative scaling and our conversion
      # factors are given for division
      result.value = 1.0 / result.value
    of utDerived:
      # convert to base & then get conversion
      let scale = t.toBaseTypeScale()
      result = t.toUnitProduct.flatten(needConversion = true).toNaturalUnitImpl()
      result.value *= scale
  of qtCompound:
    # convert to base units and then compute natural unit conversions
    let scale = t.toBaseTypeScale()
    result = t.toUnitProduct.flatten(needConversion = true).toNaturalUnitImpl()
    result.value *= scale

proc toNaturalUnitImpl(t: UnitProduct): UnitProduct =
  ## Converts a compound unit to natural units
  result = newUnitProduct()
  for unit in t.units: # note: adding units takes care of accumulating `value`
    result.add toNaturalUnitImpl(unit)

macro toNaturalUnit*[T: SomeUnit](t: T): untyped =
  ## parses the unit and converts it to natural units (`eV`) according to
  ## the contained
  var typ = t.parseDefinedUnit()
    .toNaturalUnitImpl()
  let scale = typ.value
  let resType = typ.simplify().toNimType()
  result = quote do:
    defUnit(`resType`)
    `resType`(`t`.float * `scale`)
