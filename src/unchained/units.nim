import math, macros, options, sets, tables, strutils, unicode, typetraits, strformat

## This file is the main user facing API
import core_types, ct_unit_types, macro_utils, define_units, quantities
export FloatType

const ShortFormat {.booldefine.} = true

proc pretty(x: UnitInstance, short = true): string = x.toNimType(short, internal = false)
proc pretty(x: UnitProduct, short = true): string = x.toNimTypeStr(short, internal = false)

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

## This is a *forced* conversion of a unit to a FloatType. It simply removes any unit from
## the type. Currently just `x.FloatType`. Might change in the future.
proc toFloat*[T: SomeUnit](x: T): FloatType = x.FloatType

## auto conversion of `UnitLess` to `FloatType` is possible so that e.g. `sin(5.kg / 10.kg)` works as expected!
converter toRawFloat*(x: UnitLess): FloatType = x.FloatType
converter toUnitLess*(x: SomeNumber): UnitLess = x.UnitLess
converter toUnitLess*(x: float64): UnitLess = x.UnitLess
converter toUnitLess*(x: float32): UnitLess = x.UnitLess

import hashes
proc hash*[T: SomeUnit](x: T): Hash =
  result = result !& hash($typeof(T))
  result = result !& hash(x.FloatType)
  result = !$result

## Pretty printing of units

macro shortName(t: typed): untyped =
  let typ = t.parseDefinedUnit()
  result = newLit typ.pretty(short = true)

macro unitName(t: typed): untyped =
  let typ = t.parseDefinedUnit()
  result = newLit typ.pretty(short = false)

proc prettyImpl*(s: FloatType, typStr: string, precision: int, short: bool): string =
  result = s.formatFloat(precision = precision)
  result.trimZeros()
  if not short:
    when not defined(noUnicode):
      result.add &" {typStr}"
    else:
      result.add &" {typStr}"
  else:
    when not defined(noUnicode):
      result.add &" {typStr}"
    else:
      result.add &" {typStr}"

macro unitOf*[T: SomeUnit](s: T): untyped =
  ## A helper to only get the unit name of a type *as a string*.
  ##
  ## It does not replace `typeof`, but `typeof` is not very helpful
  ## if one is interested in the name of a unit type only, because of
  ## the way the compiler treats aliases. Hence, for string names
  ## of a unit, use this.
  ##
  ## Note: behavior of this may change in the future and may currently
  ## give slightly different representations than a regular `$` call.
  let typStr = s.parseDefinedUnit().toNimTypeStr(short = ShortFormat,
                                                 internal = false)
  result = newLit typStr

macro quantityOf*[T: SomeUnit](x: T): untyped =
  ## Returns a string representation of the full quantity of the given
  ## unit in base quantities.
  ##
  ## Very useful for manual understanding / dimensional analysis.
  let xCT = x.parseDefinedUnit().toQuantityPower()
  let quantityStr = xCT.pretty()
  result = newLit quantityStr


macro `$`*[T: SomeUnit](s: T): string =
  # `$` is a macro so that we don't lose the alias type information possibly associated
  # to `s`!
  let typStr = s.parseDefinedUnit().toNimTypeStr(short = ShortFormat,
                                                 internal = false)
  result = quote do:
    prettyImpl(`s`.FloatType, `typStr`, precision = -1, short = ShortFormat)

macro pretty*[T: SomeUnit](s: T, precision: int, short: bool): untyped =
  ## Equivalent to `$`, but allows to change precision and switch to long format.
  let typStr = s.parseDefinedUnit().toNimTypeStr(short = ShortFormat,
                                                 internal = false)
  result = quote do:
    prettyImpl(`s`.FloatType, `typStr`, precision = `precision`, short = `short`)

macro defUnit*(arg: untyped, toExport: bool = false): untyped =
  ## Defines the given unit `arg` the scope. If `toExport` is `true` and the call happens
  ## at top level, the unit will be exported.
  ##
  ## This is required for any not predefined compound unit (product of different units),
  ## unless the same unit appeared "naturally" due to a math operation or `.` dot operator
  ## assigning a unit. Note however that it may still be desired to call `defUnit` manually
  ## to be certain the unit is available, as implicitly defined units are only visible
  ## in the scope 'below' their definition (which can lead to confusing errors).
  let argCT = parseDefinedUnit(arg)
  let toExport = toExport.strVal == "true"

  let resTypeCT = argCT.simplify(mergePrefixes = true)
  let resType = resTypeCT.toNimType()
  let resTypeShort = resTypeCT.toNimType(short = true)
  ## for the "base" type:
  ## - flatten: replace all compound units with their base units `iff`:
  ##   - the compound is of identity SI prefix (otherwise need a factor for SI prefix
  ##     conversion, which cannot be mapped to a *pure type*
  ##   - the compound is *not* a derived compound, i.e. eV or lbs that requires a
  ##     conversion factor
  let baseTypeCT = argCT.flatten(onlyFlattenSiIdentity = true,
                                 dontFlattenDerived = true).simplify
  let baseType = baseTypeCT.toNimType()
  let baseTypeShort = baseTypeCT.toNimType(short = true)
  let distinctQuant = nnkDistinctTy.newTree(bindSym"CompoundQuantity")
  proc emitType(typ, asTyp: NimNode, toExport: bool): NimNode =
    let expTyp = if toExport: nnkPostfix.newTree(ident"*", typ)
                 else: typ
    result = nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        expTyp,
        newEmptyNode(),
        asTyp
      )
    )
    result = quote do:
      when not declared(`typ`):
        `result`

  result = newStmtList()
  if resType.strVal != "UnitLess":
    result.add emitType(baseType,      distinctQuant, toExport)
    result.add emitType(baseTypeShort, baseType,      toExport)
    result.add emitType(resType,       baseType,      toExport)
    result.add emitType(resTypeShort,  baseType,      toExport)
    result.add emitType(arg,           baseType,      toExport)
  else:
    result.add emitType(arg,          resType, toExport)
    result.add emitType(resTypeShort, resType, toExport)

  # add new unit to `UnitTab`
  insert(arg.toStrLit.strVal, argCT)

## TODO: we should really combine these macros somewhat?
from utils import almostEqual
macro `==`*[T: SomeUnit; U: SomeUnit](x: T, y: U): bool =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    let almostEq = bindSym("almostEqual")
    result = quote do:
      `almostEq`(`x`.FloatType, `y`.FloatType)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # compare scaled to base type units
    let almostEq = bindSym("almostEqual")
    result = quote do:
      `almostEq`(`x`.FloatType * `xScale`, `y`.FloatType * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.FloatType < `y`.FloatType)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # compare scaled to base type units
    result = quote do:
      (`x`.FloatType * `xScale` < `y`.FloatType * `yScale`)
  else:
    error("Different quantities cannot be compared! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

macro `<=`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  var yCT = parseDefinedUnit(y)
  if xCT == yCT:
    result = quote do:
      (`x`.FloatType <= `y`.FloatType)
  elif xCT.commonQuantity(yCT):
    # is there a scale difference between the two types?
    let xScale = xCT.toBaseTypeScale()
    let yScale = yCT.toBaseTypeScale()
    # compare scaled to base type units
    result = quote do:
      (`x`.FloatType * `xScale` <= `y`.FloatType * `yScale`)
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
      `resType`(`xr`.FloatType + `yr`.FloatType)
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
      `resType`(`xr`.FloatType * `xScale` + `yr`.FloatType * `yScale`)
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
      `resType`(`xr`.FloatType - `yr`.FloatType)
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
      `resType`(`xr`.FloatType * `xScale` - `yr`.FloatType * `yScale`)
  else:
    error("Different quantities cannot be subtracted! Quantity 1: " & (x.getTypeInst).repr & ", Quantity 2: " & (y.getTypeInst).repr)

proc `-`*[T: SomeUnit](x: T): T = (-(x.FloatType)).T

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
      `resType`(`xr`.FloatType * `yr`.FloatType)
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
    let resType = resTypeCT.toNimType()
    if scaleOriginal != scaleConv:
      let scale = scaleOriginal / scaleConv
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.FloatType * `yr`.FloatType * `scale`)
    else:
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.FloatType * `yr`.FloatType)

#template `*`*[T: SomeUnit; U: SomeNumber](x: T; y: U{lit}): T = (x.FloatType * y.FloatType).T
#template `*`*[T: SomeUnit; U: SomeNumber](x: U{lit}; y: T): T = (x.FloatType * y.FloatType).T

macro `/`*[T: SomeUnit|SomeNumber; U: SomeUnit|SomeNumber](x: T; y: U): untyped =
  var xCT = parseDefinedUnit(x)
  let yCT = parseDefinedUnit(y)
  let xr = x.sanitizeInput()
  let yr = y.sanitizeInput()
  if xCT == yCT:
    # excactly the same type, result is simply unitless
    result = quote do:
      UnitLess(`xr`.FloatType / `yr`.FloatType)
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
    let resType = resTypeCT.toNimType()
    if scaleOriginal != scaleConv:
      let scale = scaleOriginal / scaleConv
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.FloatType / `yr`.FloatType * `scale`)
    else:
      result = quote do:
        defUnit(`resType`)
        `resType`(`xr`.FloatType / `yr`.FloatType)

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
    `resType`(sqrt(`tr`.FloatType))

proc abs*[T: SomeUnit](t: T): T = (abs(t.FloatType)).T

macro determineScale(x: typedesc, y: typedesc): FloatType =
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

macro to*[T: SomeUnit; U: SomeUnit](x: T; to: typedesc[U]): U =
  ## Converts the given unit `x` to the desired target unit `to`. The
  ## units must represent the same quantities, otherwise a CT error is
  ## thrown.
  let xCT = x.parseDefinedUnit()
  let yCT = to.parseDefinedUnit()
  if xCT == yCT:
    let resType = yCT.toNimType(short = true)
    result = quote do:
      `resType`(`x`)
  elif commonQuantity(xCT, yCT):
    # perform conversion
    ## thus determine scaling factor due to different SI prefixes
    let scale = xCT.toBaseTypeScale() / yCT.toBaseTypeScale()
    let resType = yCT.toNimType(short = true)
    result = quote do:
      `resType`(`x`.FloatType * `scale`)
  else:
    error("Cannot convert " & $T & " to " & $U & " as they represent different " &
      "quantities!")

macro toDef*[T: SomeUnit](x: T, to: untyped): untyped =
  ## A macro version of `to` above, which works for target types `to`, which
  ## have not been defined via `defUnit` yet. Calls `defUnit` and then dispatches
  ## to `to`.
  ##
  ## NOTE: Under certain use cases the order of evaluation by the Nim compiler can
  ## lead to "type mismatch" errors. For example
  ##
  ## ```nim
  ## block:
  ##   defUnit(km•h⁻¹)
  ##   proc foo[M: Mass; A: Acceleration](m: M, a: A): km•h⁻¹ =
  ##     result = (m * a).toDef(km•h⁻¹)
  ## ```
  ##
  ## in this case the compiler won't understand that `km•h⁻¹` is already defined
  ## leading to ambiguous errors ("got X, expected X = Alias").
  ##
  ## Therefore, use at your own risk. Useful in short pieces of code though!
  let toCT = parseDefinedUnit(to).toNimType(short = true)
  result = quote do:
    when not declared(`toCT`):
      defUnit(`toCT`)
    `x`.to(`toCT`)

{.experimental: "dotOperators".}
macro `.`*[T: SomeUnit|SomeNumber](x: T; y: untyped): untyped =
  ## macro to allow to generate new types on the fly
  ## TODO: maybe have an explicit distinction between already defined types
  ## and not defined types? Use `.!` or something like this instead?
  let typX = x.getTypeInst()
  let yCT = y.parseDefinedUnit()

  let simplified = yCT.simplify(mergePrefixes = true).toNimType()
  # check whether argument is actually `UnitLess` and not something that fails
  # parsing as a unit. TODO: improve unit parsing to handle this? Need a failed
  # parsing state != UnitLess
  let isUnitLess = y.strVal == "UnitLess" or
    (yCT.units.len > 1 and # > 1 means something like mol•mol⁻¹ can work
     simplified.strVal == "UnitLess")
  let rewrite = not isUnitLess and simplified.strVal == "UnitLess" # parsing failed
  let xr = x.sanitizeInput()
  if not rewrite:
    let resType = yCT.toNimType()
    result = quote do:
      when not declared(`resType`):
        defUnit(`resType`)
      `resType`(`xr`.FloatType)
  else:
    # parsing as a unit failed, rewrite to get possible CT error or correct result
    result = quote do:
      `y` `x`

macro toBaseUnits*[T: SomeUnit](x: T): untyped =
  ## Converts the given input unit to the fully flattened base type.
  ##
  ## This is mostly a convenient tool if one wishes to quickly check the
  ## pure base unit representation of a unit.
  let xCT = x.parseDefinedUnit()
  let resType = xCT.flatten().toBaseType(true).simplify(mergePrefixes = true).toNimType
  result = quote do:
    `x`.toDef(`resType`)

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
        #result.value = pow(result.value * u.prefix.toFactor(), u.power.FloatType)
        uPower = u.power
        u.power *= t.power
      # Note: we do *not* multiply in the power of the natural unit, as this is
      # only related to the dimension of the resulting unit, but our conversions are
      # for the correct dimension already
      result.value = pow(factor / t.prefix.toFactor(), t.power.FloatType)
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
  result = initUnitProduct()
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
    `resType`(`t`.FloatType * `scale`)
