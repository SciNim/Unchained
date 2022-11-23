# stdlib
import std / [macros, strutils, unicode, typetraits, parseutils, math, sequtils, options]
# local files
import core_types, ct_unit_types, macro_utils

from std / tables import getOrDefault, `[]`

proc parseSiPrefixShort(c: char): SiPrefix =
  ## For the case of short SI prefixes (i.e. single character) return it
  result = SiShortPrefixStrTable.getOrDefault($c, siIdentity) # initialize with identity, in case none match

proc parseSiPrefixLong(s: string): SiPrefix =
  ## For the case of short SI prefixes (i.e. single character) return it
  result = SiPrefixStrTable.getOrDefault(s, siIdentity) # initialize with identity, in case none match

func getDigit(digits: openArray[Rune], d: Rune): int =
  for idx, digit in digits:
    if digit == d: return idx

proc lookupUnit(tab: UnitTable, s: string): DefinedUnit =
  ## looks up the given predefined unit in our CT tables of known units
  result = tab[s]

proc tryLookupUnit(tab: UnitTable, s: string): Option[DefinedUnit] =
  ## looks up the given predefined unit in our CT tables of known units
  if s in tab:
    result = some(tab[s])

proc parsePrefixAndUnit(tab: UnitTable, x: string, start, stop: int):
                       tuple[prefix: SiPrefix, unit: DefinedUnit] =
  ## Handles parsing of a unit in the range `start -> stop` in `x` and
  ## returns both the prefix as well as the unit itself.
  # NOTE: can we avoid the string slice copies? :/
  result.prefix = siIdentity
  case stop - start
  of 0: # invalid
    doAssert false
  of 1: # short unit without SI prefix
    # must be short unit, e.g. `J`
    result.unit = tab.getShort($x[start])
  of 2:
    # short unit with prefix (this means disallow long versions with length
    # shorter than 3, unless we added a `isUpper` check
    # or short unit without prefix, e.g. `Bq`
    #if x[start].isUpper: # possibly check for long unit
    let unitOpt = tab.tryLookupUnit(x[start ..< stop])
    if unitOpt.isSome:
      result.unit = unitOpt.get
    else:
      # first char must be short prefix & second a short unit, e.g. `mN`
      result.prefix = parseSiPrefixShort(x[start])
      result.unit = tab.getShort($x[stop-1])
  else:
    # try any unit
    var unitOpt = tab.tryLookupUnit(x[start ..< stop])
    if unitOpt.isSome:
      result.unit = unitOpt.get
    else:
      # try short prefix + short unit, i.e. parsing of unit from 1st char, e.g. `mPa`
      unitOpt = tab.tryLookupUnit(x[start+1 ..< stop])
      if unitOpt.isSome:
        result.unit = unitOpt.get
        result.prefix = parseSiPrefixShort(x[start]) # in this case prefix must be short
      else:
        # must be long + long, e.g. `KiloGram`
        # must have prefix, thus parse until upper, that defines prefix & unit
        ## XXX: fix this parse until from second character somehow
        var prefixStr = newStringOfCap(100)
        let prefixNum = parseUntil(x, prefixStr, until = {'A' .. 'Z'}, start = start+1) # long names must use ASCII!
        # look up long prefix
        result.prefix = parseSiPrefixLong(x[start] & prefixStr)
        result.unit = tab.lookupUnit(x[start + prefixNum + 1 ..< stop])

template addUnit(): untyped {.dirty.} =
  ## Dirty template used in both parsing procedures (unicode & ascii)
  ## performing the actual parsing after we've seen a
  ## separator as well as after the loop
  let (prefix, unit) = tab.parsePrefixAndUnit(x, start, stop)
  exp = if exp == 0: 1 else: exp
  exp = if negative: -exp: else: exp
  result.add newUnitInstance("", unit, exp, prefix)

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
  result = initUnitProduct()
  if x == "UnitLess": return
  const sepRune = UnicodeSep.runeAt(0)
  var
    idx = 0
    rune: Rune
    exp = 0
    negative = false
    start = 0
    stop = 0

  # Parse by walking full unit front to back, rune by rune
  while idx < x.len:
    fastRuneAt(x, idx, rune)
    case rune
    of sepRune: # found separator, parse previous unit
      addUnit()
      start = idx # new unit starts here (idx now after `sep`)
      exp = 0     # and reset exp
      negative = false
    of MinusRune:
      negative = true # register as negative and move exponent start to after
    of digitsRunes:
      # register more exponents, get current digit and shift existing exponent
      let digit = digitsRunes.getDigit(rune)
      exp = exp * 10 + digit
    else:
      # accumulate unit name
      stop = idx
  addUnit()

proc parseDefinedUnitAscii(tab: UnitTable, x: string): UnitProduct =
  result = initUnitProduct()
  if x == "UnitLess": return
  var
    idx = 0
    c: char
    exp = 0
    negative = false
    start = 0
    stop = 0

  # Parse by walking full unit front to back, rune by rune
  while idx < x.len:
    c = x[idx]
    case c
    of AsciiSep: # found separator, parse previous unit
      addUnit()
      start = idx+1 # new unit starts here (idx now after `sep`)
      exp = 0       # and reset exp
      negative = false
    of '^': # next char starts an exponent, but nothing to do here
      discard
    of '-':
      negative = true # register as negative and move exponent start to after
    of {'0' .. '9'}:
      # register more exponents, get current digit and shift existing exponent
      let digit = ord(c) - 0x30 # get digit based on character & offset in ASCII
      exp = exp * 10 + digit
    else:
      # accumulate unit name
      stop = idx+1
    inc idx
  addUnit()

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

proc tryLookupUnitType*(tab: UnitTable, n: NimNode): Option[UnitProduct] =
  ## This first `getUnitType` tries to see if we already know this unit.
  ## In that case, we can just return it instead of parsing it again.
  ## This is especially useful for predefined aliases like `N = Newton = KiloGram•Meter•Second⁻²`
  ## as we otherwise fully resolve it to the long format.
  proc fromTab(tab: UnitTable, nStr: string): Option[UnitProduct] =
    if tab.isUserDefined(nStr):
      result = some(tab.getUserDefined(nStr))
    elif nStr in tab:
      result = some(tab[nStr].toUnitInstance(assignPrefix = false).toUnitProduct())

  case n.kind
  of nnkIdent:
    result = fromTab(tab, n.strVal)
  of nnkAccQuoted:
    var s: string
    for el in n:
      s.add el.strVal
    result = fromTab(tab, s)
  of nnkSym:
    let nTyp = n.getTypeInst
    var nStr: string
    case nTyp.kind
    of nnkBracketExpr: nStr = nTyp[1].strVal
    of nnkDistinctTy: nStr = nTyp[1].strVal
    of nnkSym: nStr = nTyp.strVal
    else: error("Invalid node for type : " & nTyp.repr)
    if nStr in ["float", "float64", "int", "int64", "UnitLess"]:
      result = some(initUnitProduct())
    else:
      result = fromTab(tab, nStr)
  of nnkTypeOfExpr:
    result = tab.tryLookupUnitType(n[0])
  else:
    if n.typeKind != ntyNone:
      let nTyp = n.getTypeInst
      result = tab.tryLookupUnitType(nTyp)

proc parseDefinedUnit*(tab: var UnitTable, x: NimNode): UnitProduct =
  result = initUnitProduct()
  # first check if part of previously user defined units
  let resOpt = tab.tryLookupUnitType(x)
  if resOpt.isSome:
    # as we lookup a unit from the `UnitTable` and we have a direct match for a unit
    #, we do *not* want to assigne the base prefix as we have the literal unit, not its base.
    result = resOpt.get
  else:
    # have to fully parse it
    let xTyp = getUnitType(x)
    var xT = xTyp.strVal
    result = tab.parseDefinedUnit(xT)

    # Insert the newly found unit into the table
    tab.insert(xT, result)
