# stdlib
import std / [macros, strutils, unicode, typetraits, strformat, parseutils, math, sequtils, options]
# local files
import core_types, ct_unit_types, macro_utils

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
  result = siIdentity # initialize with identity, in case none match
  for (el, prefix) in SiPrefixStringsShort:
    if $c == el: return prefix

proc startsAsKnownUnit(tab: UnitTable, s: string): bool =
  ## Checks if the given string stars like a known unit (short or long version)
  for short in shortNames(tab):
    if s.startsWith(short): return true
  for long in longNames(tab):
    if s.startsWith(long): return true

proc isLongBaseUnit(tab: UnitTable, s: string): bool =
  for b in longBaseNames(tab):
    if s.startsWith($b): return true

proc parseSiPrefix(tab: UnitTable, s: var string): SiPrefix =
  ## Return early if we only have on rune in total or until we reach
  ## a `⁻` or any of the superscript numbers. Important for things like
  ## `m`. Only a prefix if there's more than one rune.
  if s.runeLen == 1 or s.runeAt(1).toUtf8() in digitsAndMinus: return siIdentity
  result = siIdentity
  if s.runeAt(0).isUpper:
    if tab.isLongBaseUnit(s): return siIdentity # if it's Meter, Gram etc.
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
  result = parseSiPrefixShort(s[0])
  if result != siIdentity: # if `result` is siIdentity, `parseSiPrefixShort` failed to
                           # match any short prefix. Hence we look at a unit
    if tab.startsAsKnownUnit(s[1 ..< s.len]):
      s = s[1 ..< s.len]   # keep result as found prefix, remove it from `s`
    else:
      result = siIdentity  # did not find any prefixes, as rest of input is *not* a unit,
                           # which means full `s` must be a unit.

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

proc lookupUnit(tab: UnitTable, s: string): DefinedUnit =
  ## looks up the given predefined unit in our CT tables of known units
  result = tab[s]

proc parseDefinedUnitUnicode(tab: UnitTable, x: string): UnitProduct =
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
  result = newUnitProduct()
  if x == "UnitLess": return
  let xTStrs = if "•" in x: x.split("•")
               else: x.split("·")
  for el in xTStrs:
    var mel = el
    let prefix = tab.parseSiPrefix(mel)
    let negative = hasNegativeExp(mel)
    let exp = parseExponent(mel, negative)
    let unit = tab.lookupUnit(mel)
    let ctUnit = newUnitInstance(el, unit, exp, prefix)
    result.units.add ctUnit

proc parseDefinedUnitAscii(tab: UnitTable, x: string): UnitProduct =
  result = newUnitProduct()
  var idx = 0
  var buf: string
  while idx < x.len:
    idx += x.parseUntil(buf, until = '*', start = idx)
    let powIdx = buf.find("^")
    doAssert powIdx < buf.high, "Invalid unit, ends with `^`: " & $buf
    let exp = if powIdx > 0: parseInt(buf[powIdx + 1 .. buf.high])
              else: 1
    let el = if powIdx > 0: buf[0 ..< powIdx]
             else: buf
    var mel = el
    let prefix = tab.parseSiPrefix(mel)
    let unit = tab.lookupUnit(mel)
    let ctUnit = newUnitInstance(el, unit, exp, prefix)
    result.add ctUnit
    inc idx

proc parseDefinedUnit*(tab: UnitTable, s: string): UnitProduct =
  ## TODO: avoid walking over `s` so many times!
  if "•" in s or "·" in s or digitsAndMinus.anyIt(it in s):
    result = tab.parseDefinedUnitUnicode(s)
  elif "*" in s or s.anyIt(it in AsciiChars):
    result = tab.parseDefinedUnitAscii(s)
  # TODO: add verbose mode
  #elif "Per" in s:
  #  result = parseDefinedUnitVerbose(x)
  else:
    # else does not matter which proc, because it should be a single unit, e.g. `KiloGram`
    result = tab.parseDefinedUnitUnicode(s)

proc tryLookupUnitType*(tab: UnitTable, n: NimNode): Option[DefinedUnit] =
  ## This first `getUnitType` tries to see if we already know this unit.
  ## In that case, we can just return it instead of parsing it again.
  ## This is especially useful for predefined aliases like `N = Newton = KiloGram•Meter•Second⁻²`
  ## as we otherwise fully resolve it to the long format.
  case n.kind
  of nnkIdent:
    if n.strVal in tab:
      result = some(tab[n.strVal])
  of nnkSym:
    let nTyp = n.getTypeInst
    var nStr: string
    case nTyp.kind
    of nnkBracketExpr: nStr = nTyp[1].strVal
    of nnkDistinctTy: nStr = nTyp[1].strVal
    of nnkSym: nStr = nTyp.strVal
    else: error("Invalid node for type : " & nTyp.repr)
    if nStr in tab:
      result = some(tab[nStr])
  of nnkTypeOfExpr:
    result = tab.tryLookupUnitType(n[0])
  else:
    if n.typeKind != ntyNone:
      let nTyp = n.getTypeInst
      result = tab.tryLookupUnitType(nTyp)

proc parseDefinedUnit*(tab: UnitTable, x: NimNode): UnitProduct =
  result = newUnitProduct()
  if x.isUnitLessNumber:
    #let ctUnit = newUnitLess()
    #result.add ctUnit
    return result
  let resOpt = tab.tryLookupUnitType(x)
  if resOpt.isSome:
    # as we lookup a unit from the `UnitTable` and we have a direct match for a unit
    #, we do *not* want to assigne the base prefix as we have the literal unit, not its base.
    result = resOpt.get.toUnitInstance(assignPrefix = false).toUnitProduct
  else:
    # have to fully parse it
    let xTyp = getUnitType(x)
    var xT = xTyp.strVal
    result = tab.parseDefinedUnit(xt)
    ## TODO: add new found unit to `UnitTable` in a separate table?
