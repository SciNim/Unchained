import std / [macros, tables, strutils, math, sequtils, sets, algorithm]

import core_types, quantities, ct_unit_types, utils

type
  DefinedUnits* = seq[DefinedUnit] #object

  ## XXX: Ideally we would replace the QuantityTab Table by a CacheTable. But then we need to
  ## use `newLit` to store a `CTQuantity` in it, which means on the "other side" (i.e. here)
  ## we need to again parse the AST into an object?
  #const QuantityTab = CacheTable"QuantityTab"

  UnitTable = object
    # quantity stores the *base unit* referring to a quantity (the key)
    quantity: Table[string, int]
    # long and short store indices to the `seq` `units`
    long: Table[string, int]
    short: Table[string, int]
    units: seq[DefinedUnit]

proc initUnitTable(): UnitTable =
  result = UnitTable(quantity: initTable[string, int](),
                     long: initTable[string, int](),
                     short: initTable[string, int](),
                     units: newSeq[DefinedUnit]())

proc len(tab: UnitTable): int = tab.units.len

proc contains(tab: UnitTable, u: string): bool =
  ## Checks if the given `u` is in the `UnitTable`
  result = u in tab.short or u in tab.long

proc insert(tab: var UnitTable, u: DefinedUnit) =
  ## Inserts the given `u` into the `UnitTable`. The correct sub field will be filled
  ## based on `isLong`
  let idx = tab.len # get index as the current length
  if u.kind == utBase:
    let qName = u.quantity.getName()
    if qName in tab.quantity:
      error("The unit " & $u.name & " defines a quantity " & $qName & " that was already " &
        "defined by another unit: " & $tab.units[tab.quantity[qName]].repr & ". If this is not a " &
        "base unit, make sure to define a `conversion` to the base unit of the same quantity.")
    tab.quantity[qName] = idx

  tab.long[u.name] = idx
  tab.short[u.short] = idx
  tab.units.add u

proc `[]`(tab: UnitTable, s: string): DefinedUnit =
  if s in tab.long:
    result = tab.units[tab.long[s]]
  elif s in tab.short:
    result = tab.units[tab.short[s]]
  else:
    raise newException(KeyError, "Given unit " & $s & " not known in `UnitTable`.")

proc `[]`(tab: UnitTable, q: CTQuantity): DefinedUnit =
  let idx = tab.quantity[q.getName()]
  result = tab.units[idx]

proc `[]`(tab: UnitTable, q: CTBaseQuantity): DefinedUnit =
  let idx = tab.quantity[q.name]
  result = tab.units[idx]

proc getIdx(tab: UnitTable, u: UnitInstance): int =
  ## Returns the index (i.e. priority) of the given unit instance
  let s = u.unit.name
  result = tab.long[s]

var ShortUnits {.compileTime.} = newSeq[string]()
var LongUnits {.compileTime.} = newSeq[string]()
var LongBaseUnits {.compileTime.} = newSeq[string]()
var UnitTab {.compileTime.} = initUnitTable()

proc isLongBaseUnit(s: string): bool =
  for b in LongBaseUnits:
    if s.startsWith($b): return true

proc `<`*(a, b: UnitInstance): bool =
  ## Comparison based on the order in `UnitTab`.
  let aIdx = UnitTab.getIdx(a)
  let bIdx = UnitTab.getIdx(b)
  if aIdx < bIdx:
    result = true
  elif aIdx > bIdx:
    result = false
  else:
    ## NOTE: the power seem "inverted". This is because we wish to have units with
    ## larger powers ``in front`` of units with smaller powers. E.g.
    ## `Meter•Meter⁻¹` instead of `Meter⁻¹•Meter`
    ## We cannot sort in descending order, because the actual units in the `UnitKind`
    ## enum needs to be respected.
    if a.power > b.power:
      result = true
    elif a.power < b.power:
      result = false
    else:
      result = a.prefix < b.prefix



## XXX: clean up this mess. Parsing shouldn't be duplicated here and in main file etc









import std / [macros, strutils, unicode, typetraits, strformat, parseutils]

proc parseUntil(s: string, chars, errorOn: openArray[string]): int =
  ## parses until one of the runes in `chars` is found
  var idx = 0
  var rune: Rune
  var oldIdx = idx
  while idx < s.len:
    oldIdx = idx
    fastRuneAt(s, idx, rune)
    if rune.toUtf8 in chars:
      return oldIdx
    elif rune.toUtf8 in errorOn:
      error("Invalid rune in input string: " & $(rune.toUtf8()) & ". Did you type " &
        "a non superscript power by accident?")
  when false:
    for rune in utf8(s):
      if rune in chars:
        return idx
      inc idx
  # didn't find it
  result = -1

proc parseSiPrefixShort(c: char): SiPrefix =
  ## For the case of short SI prefixes (i.e. single character) return it
  for (el, prefix) in SiPrefixStringsShort:
    if $c == el: return prefix

proc startsAsKnownUnit(s: string): bool =
  ## Checks if the given string stars like a known unit (short or long version)
  for short in ShortUnits:
    if s.startsWith(short): return true
  for long in LongUnits:
    if s.startsWith(long): return true

proc parseSiPrefix(s: var string): SiPrefix =
  ## Return early if we only have on rune in total or until we reach
  ## a `⁻` or any of the superscript numbers. Important for things like
  ## `m`. Only a prefix if there's more than one rune.
  if s.runeLen == 1 or s.runeAt(1).toUtf8() in digitsAndMinus: return siIdentity
  result = siIdentity
  if s.runeAt(0).isUpper:
    if s.isLongBaseUnit: return siIdentity # if it's Meter, Gram etc.
    ## try to find Long Si prefix
    for (el, prefix) in SiPrefixStringsLong:
      if prefix == siIdentity: continue
      if s.startsWith(el):
        s.removePrefix(el)
        return prefix
  ## no is upper does not mean it might not short, since some are upper
  ## else check for short prefix.
  # Alternative:
  # - lookup first characer & map to SI prefix
  # - it *is* that prefix iff what comes after is a valid unit
  # how to break tie between `T` (= tesla) and `TT` (= Tera Tesla) ?
  let short = parseSiPrefixShort(s[0])
  if startsAsKnownUnit(s[1 ..< s.len]):
    s = s[1 ..< s.len]
    result = short
  else:
    result = siIdentity

proc hasNegativeExp(s: var string): bool =
  var rune: Rune
  var idx = 0
  var oldIdx = idx
  while idx < s.len:
    oldIdx = idx
    fastRuneAt(s, idx, rune)
    if rune.toUtf8() == "⁻":
      s.delete(oldIdx, idx - 1)
      return true

proc parseExponent(s: var string, negative: bool): int =
  var buf: string
  let idxStart = s.parseUntil(digits, errorOn = DigitsAscii)
  var idx = idxStart
  if idx > 0:
    let numDigits = s[idx .. ^1].runeLen
    var seen = 0
    while idx < s.len:
      var buf: Rune
      fastRuneAt(s, idx, buf)
      inc seen
      let val = digits.find(buf.toUtf8()) * 10^(numDigits - seen)
      result += val
    result = if negative: -result else: result
    # remove `idx` onwards from s
    s.delete(idxStart, s.len)
  else:
    # no exponent means `1`
    result = 1

#proc getUnitType(n: NimNode): NimNode =
#  case n.kind
#  of nnkIdent: result = n
#  of nnkAccQuoted:
#    var s: string
#    for el in n:
#      s.add el.strVal
#    result = ident(s)
#  else: result = n.getTypeInst.getUnitTypeImpl()

proc lookupUnit(s: string): DefinedUnit =
  ## looks up the given predefined unit in our CT tables of known units
  result = UnitTab[s]

proc parseDefinedUnitUnicode(x: string): UnitProduct =
  ## 1. split by `•`
  ## for el in splits
  ##   2. parse possible si prefix
  ##   2a. remove prefix from string
  ##   3. parse possible negative unicode char
  ##   3a. parse possible exponent
  ##   4. parse name of unit
  ## Complication: We have 3 different notations for units
  ## a) m, kg, m•s⁻², ...
  ## b) Meter, Kg, Meter•Second⁻², ...
  ## c) "meter per second squared" -> "meterPerSecondSquared"
  ## a) and b) can be parsed together by both looking for `m` as well as `Meter` in each
  ## element. Verbose always start capital letters, shorthand depending on SI prefix / unit (N, V, A...)
  let xTStrs = if "•" in x: x.split("•")
               else: x.split("·")
  result = UnitProduct()
  for el in xTStrs:
    var mel = el
    let prefix = mel.parseSiPrefix
    let negative = hasNegativeExp(mel)
    let exp = parseExponent(mel, negative)
    let unit = lookupUnit(mel)
    let ctUnit = newUnitInstance(el, unit, exp, prefix)
    result.units.add ctUnit

#proc parseDefinedUnitAscii(x: string): UnitProduct =
#  var idx = 0
#  var buf: string
#  while idx < x.len:
#    idx += x.parseUntil(buf, until = '*', start = idx)
#    let powIdx = buf.find("^")
#    doAssert powIdx < buf.high, "Invalid unit, ends with `^`: " & $buf
#    let exp = if powIdx > 0: parseInt(buf[powIdx + 1 .. buf.high])
#              else: 1
#    var mel = if powIdx > 0: buf[0 ..< powIdx]
#              else: buf
#    let prefix = mel.parseSiPrefix
#    let unitKind = parseUnitKind(mel)
#    let isShortHand = if parseEnum[UnitKind](mel, ukUnitLess) == ukUnitLess and mel != "UnitLess": true else: false
#    let ctUnit = initDefinedUnit(buf, unitKind, exp, prefix,
#                            isShortHand = isShortHand)
#    result.add ctUnit
#    inc idx

proc parseDefinedUnit*(s: string): UnitProduct =
  ## TODO: avoid walking over `s` so many times!
  if "•" in s or digitsAndMinus.anyIt(it in s):
    result = parseDefinedUnitUnicode(s)
  elif "*" in s or s.anyIt(it in AsciiChars):
    #result = parseDefinedUnitAscii(s)
    discard
  # TODO: add verbose mode
  #elif "Per" in s:
  #  result = parseDefinedUnitVerbose(x)
  else:
    # else does not matter which proc, because it should be a single unit, e.g. `KiloGram`
    result = parseDefinedUnitUnicode(s)

#proc parseDefinedUnit(x: NimNode): UnitProduct =
#  if x.isUnitLessNumber:
#    let ctUnit = initDefinedUnit(x.getTypeImpl.repr, ukUnitLess, 1, siIdentity)
#    result.add ctUnit
#    return result
#  let xTyp = x.getUnitType
#  var xT = xTyp.strVal
#  ## TODO: avoid walking over `xT` so many times!
#  result = xt.parseDefinedUnit()














proc parseShort(n: NimNode): string =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = n[0].strVal

proc parseQuantity(n: NimNode): CTQuantity =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = QuantityTab[n[0].strVal]

proc parseConversion(n: NimNode): UnitProduct = ## DefinedUnitValue !
  echo n.treerepr
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind in {nnkDotExpr}
  let dotEx = n[0]
  result = parseDefinedUnit(dotEx[1].strVal)
  case dotEx[0].kind
  of nnkIntLit: result.value = float(dotEx[0].intVal)
  of nnkFloatLit: result.value = dotEx[0].floatVal
  else: error("Invalid node for unit conversion definition " & $dotEx.repr & ".")

proc parsePrefix(n: NimNode): SiPrefix =
  doAssert n.len == 1 and n.kind == nnkStmtList and n[0].kind == nnkIdent
  result = parseEnum[SiPrefix](n[0].strVal)

proc assertOption(n: NimNode): bool =
  result = n.kind == nnkCall and
    n.len == 2 and
    n[0].kind == nnkIdent and
    n[1].kind == nnkStmtList

proc parseUnit(n: NimNode): DefinedUnit =
  ## Parses:
  ##
  ##  Call
  ##    Ident "Meter"
  ##    StmtList
  ##      Call
  ##        Ident "short"
  ##        StmtList
  ##          Ident "m"
  ##      Call
  ##        Ident "quantity"
  ##        StmtList
  ##          Ident "Length"
  if not assertOption(n):
    error("Invalid field " & $n.repr & " in `declareUnits` definining a unit.")
  let name = n[0].strVal
  var
    short: string
    quantity: CTQuantity
    conversion: UnitProduct
    prefix: SiPrefix = siIdentity # identity if not otherwise specified
  for arg in n[1]: # parse the stmt list, i.e. the "options"
    if not assertOption(arg):
      error("Invalid field " & $arg.repr & " in `declareUnits` setting a unit option.")
    let field = arg[0].strVal
    case field
    of "short": short = parseShort(arg[1])
    of "quantity": quantity = parseQuantity(arg[1])
    of "conversion": conversion = parseConversion(arg[1])
    of "prefix": prefix = parsePrefix(arg[1])
    else: error("Invalid unit option " & $field & " defining the unit: " & $result.name & ".")
  ShortUnits.add short
  LongUnits.add name
  if not conversion.isNil:
    # is *not* a base unit (inch, Liter, PoundForce, ..., something that needs conversion)
    result = DefinedUnit(kind: utDerived,
                         name: name,
                         basePrefix: prefix,
                         short: short,
                         quantity: quantity,
                         conversion: conversion)
  else:
    # *is* a base unit (but possibly compound!)
    LongBaseUnits.add name
    result = DefinedUnit(kind: utBase,
                         name: name,
                         basePrefix: prefix,
                         short: short,
                         quantity: quantity)
  UnitTab.insert(result)

proc parseUnits(n: NimNode): DefinedUnits =
  ## Handles parsing the given base units or derived units
  ##
  ##    StmtList
  ##      Call
  ##        Ident "Meter"
  ##        ...
  ##      Call
  ##        Ident "Second"
  ##        ...
  ##
  ## Parses the given calls defining the units.
  doAssert n.kind == nnkStmtList
  for unit in n:
    result.add parseUnit(unit)

proc parseCall(c: NimNode): DefinedUnits =
  ## Handles dispatching based on base units / derived ident
  ##
  ##  Call
  ##    Ident "BaseUnits" # or "Derived"
  ##    StmtList
  ##      Call
  ##        ...
  doAssert c.kind == nnkCall
  doAssert c.len == 2
  doAssert c[1].kind == nnkStmtList
  var baseUnits: DefinedUnits
  var derivedUnits: DefinedUnits
  if c[0].kind == nnkIdent and c[0].strVal == "BaseUnits":
    baseUnits = parseUnits(c[1])
  elif c[0].kind == nnkIdent and c[0].strVal == "Derived":
    derivedUnits = parseUnits(c[1]) ## FIXME: hand `baseUnits` to above procs to lookup conversions? baseUnits)
  result.add baseUnits
  result.add derivedUnits

proc toNimType(u: UnitInstance, short = false): string =
  #if u.unitKind == ukUnitLess: return
  ## XXX: handle base prefix of base units!
  let siPrefixStr = if short: SiShortPrefixTable[u.prefix]
                    else: SiPrefixTable[u.prefix]
  result = siPrefixStr
  if not short:
    result.add u.unit.name
  else:
    result.add u.unit.short
  if u.power < 0:
    result.add "⁻"
  if u.power > 1 or u.power < 0:
    for digit in getPow10Digits(u.power):
      result.add digits[digit]

proc toNimTypeStr(x: UnitProduct, short = false): string =
  ## converts `x` to the correct string representation
  # return early if no units in x
  if x.units.len == 0: return "UnitLess"
  ## XXX: add a `reduce` call / simplify
  let xSorted = x.units.sorted
  for idx, u in xSorted:
    #if u.unitKind == ukUnitLess: continue
    var str = toNimType(u, short)
    if idx < xSorted.high:
      str.add "•"
    result.add str

proc toBaseUnit(q: CTBaseQuantity): DefinedUnit =
  result = UnitTab[q]

proc toBaseUnits(q: CTQuantity): UnitProduct =
  ## Turns the given quantity into a `UnitProduct` of base units
  case q.kind
  of qtCompound:
    result = UnitProduct()
    for b in q.baseSeq:
      let baseUnit = b.quant.toBaseUnit()
      let power = b.power
      result.units.add newUnitInstance(
        q.getName(), baseUnit, power, baseUnit.basePrefix
      )
  else: discard

proc genUnitTypes(units: DefinedUnits): NimNode =
  ## Generates all unit type definitions.
  ##
  ## In the general case, e.g. for `Newton`:
  ##
  ##  type
  ##    # maybe this won't be needed anymore?
  ##    KiloGram•Meter•Second⁻²* = distinct Force
  ##    Newton* = KiloGram•Meter•Second⁻²
  ##    N* = Newton
  ##
  ## And for base units:
  ##
  ##  type
  ##    Meter* = distinct Length
  ##    m* = Meter
  ##
  ## This is because compound units are not necessarily unique.
  result = nnkTypeSection.newTree()
  # 1. generate all units for all compound quantities
  var generatedCompounds = initHashSet[string]()
  for quant in compoundQuantities(QuantityTab):
    let typ = quant.toBaseUnits().toNimTypeStr()
    let typShort = quant.toBaseUnits().toNimTypeStr(short = true)
    if typ notin generatedCompounds:
      result.add defineDistinctType(typ, quant.getName())
      result.add defineType(typShort, typ)
      generatedCompounds.incl typ

  for unit in units:
    let asBase = unit.quantity.toBaseUnits()
    if not asBase.isNil and unit.kind == utBase: # not a unit with a conversion
      # this is a base compound unit without a conversion
      # 1. generate the alias for the long name
      let compound = asBase.toNimTypeStr()
      result.add defineType(unit.name, compound)
      # 2. generate the short name alias
      result.add defineType(unit.short, unit.name)
    else:
      # 1. generate main definition, i.e. long name = distinct quantity
      result.add defineDistinctType(unit.name, unit.quantity.getName())
      # 2. generate the short name
      result.add defineType(unit.short, unit.name)

  echo result.repr

macro declareUnits*(defs: untyped): untyped =
  ## Declares a set of base and derived units.
  ##
  ## NOTE: the order in which the units are defined is their priority in terms of
  ## generated units. E.g. if you define `Gram` before `Meter`, then every auto generated
  ## unit will be written with `Gram•...•Meter•...•`.
  ##
  ## XXX: Currently we only generate a single compound quantity of one specific
  ## dimensionality (e.g. Second⁻¹ for both Frequency and Activity). The order depends
  ## on the definition of the quantities in the `declareQuantity` macro call.
  ##
  ##   This means that e.g. Becquerel is actually a `Frequency` now, as it is an
  ##   alias to Second⁻¹, which is `distinct Frequency` due to being first in the
  ##   macro call.... How can we better handle this? In practice this shouldn't really
  ##   matter though (it was the same before anyway)
  var units: DefinedUnits
  for call in defs:
    if call.kind == nnkCall:
      units.add parseCall(call)
    else:
      error("Invalid node kind " & $call.kind & " for `declareUnits` definition.")
  result = newStmtList()
  # 1. generate type definitions
  result.add genUnitTypes(units)


  # generate
  # DerivedSiUnits = Newton | Joule

  # shorthand types
  # generate
  # m = Meter
  # s = Second
  # A = Ampere
  # g = Gram
  # Kg = KiloGram
  # kg = Kg ## XXX: This should go, but it's a good case to think about how to resolve aliases!
  # ...


  # generate BaseUnitKind and UnitKind enums
