## These are our absoluteley fundamental base types
## Everything else is generated on top of these as distinct versions
## of them.
import std / tables

const FloatBytes* {.intdefine.} = 8

when FloatBytes == 8:
  type
    FloatType* = float
elif FloatBytes == 4:
  type
    FloatType* = float32
else:
  {.error: "Unsupported size for the underlying float type: " & $FloatBytes & " bytes.".}

type
  Unit* = distinct FloatType

  Quantity* = distinct Unit
  CompoundQuantity* = distinct Quantity

  Dimensionless* = distinct Quantity
  UnitLess* = distinct Dimensionless

  SiPrefix* = enum
    siQuecto, siRonto, siYocto, siZepto, siAtto, siFemto, siPico, siNano, siMicro, siMilli, siCenti, siDeci,
    siIdentity,
    siDeca, siHecto, siKilo, siMega, siGiga, siTera, siPeta, siExa, siZetta, siYotta, siRonna, siQuetta

from std / unicode import runeAt, Rune
proc toRunes(ar: static openArray[string]): auto =
  result = default(array[ar.len, Rune])
  for i, d in ar:
    result[i] = d.runeAt(0)

const digits* = ["⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]
const digitsRunes* = digits.toRunes()
const digitsAndMinus* = ["⁻","⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]
const digitsAndMinusRunes* = digitsAndMinus.toRunes()
const DigitsAscii* = ["0","1","2","3","4","5","6","7","8","9"]
const DigitsAsciiRunes* = DigitsAscii.toRunes()
const AsciiChars* = {'*', '^', '-', '0' .. '9'}
const UnicodeMinus* = "⁻"
const MinusRune* = UnicodeMinus.runeAt(0)
const AsciiSep* = '*'
const UnicodeSep* {.strdefine.} = "•"
const AsciiSepRune* = "*".runeAt(0)
const AsciiPowerRune* = "^".runeAt(0)
const AsciiMinusRune* = "-".runeAt(0)

const SiPrefixStringsLong* = {
  "Quecto" :   siQuecto,
  "Ronto"  :    siRonto,
  "Yocto"  :    siYocto,
  "Zepto"  :    siZepto,
  "Atto"   :     siAtto,
  "Femto"  :    siFemto,
  "Pico"   :     siPico,
  "Nano"   :     siNano,
  "Micro"  :    siMicro,
  "Milli"  :    siMilli,
  "Centi"  :    siCenti,
  "Deci"   :     siDeci,
  ""       : siIdentity,
  "Deca"   :     siDeca,
  "Hecto"  :    siHecto,
  "Kilo"   :     siKilo,
  "Mega"   :     siMega,
  "Giga"   :     siGiga,
  "Tera"   :     siTera,
  "Peta"   :     siPeta,
  "Exa"    :      siExa,
  "Zetta"  :    siZetta,
  "Yotta"  :    siYotta,
  "Ronna"  :    siRonna,
  "Quetta" :   siQuetta
}

const SiPrefixStringsShort* = {
  "q"  :    siQuecto,
  "r"  :     siRonto,
  "y"  :     siYocto,
  "z"  :     siZepto,
  "a"  :      siAtto,
  "f"  :     siFemto,
  "p"  :      siPico,
  "n"  :      siNano,
  "μ"  :     siMicro,
  "m"  :     siMilli,
  "c"  :     siCenti,
  "d"  :      siDeci,
  ""   :  siIdentity,
  "da" :      siDeca,
  "h"  :     siHecto,
  "k"  :      siKilo,
  "M"  :      siMega,
  "G"  :      siGiga,
  "T"  :      siTera,
  "P"  :      siPeta,
  "E"  :       siExa,
  "Z"  :     siZetta,
  "Y"  :     siYotta,
  "R"  :     siRonna,
  "Q"  :    siQuetta
}

const SiPrefixTable* = block:
  var tab = initTable[SiPrefix, string]()
  for (key, val) in SiPrefixStringsLong:
    tab[val] = key
  tab

const SiPrefixStrTable* = block:
  var tab = initTable[string, SiPrefix]()
  for (key, val) in SiPrefixStringsLong:
    tab[key] = val
  tab

const SiShortPrefixTable* = block:
  var tab = initTable[SiPrefix, string]()
  for (key, val) in SiPrefixStringsShort:
    tab[val] = key
  tab

const SiShortPrefixStrTable* = block:
  var tab = initTable[string, SiPrefix]()
  for (key, val) in SiPrefixStringsShort:
    tab[key] = val
  tab

proc `$`*(prefix: SiPrefix): string =
  result = SiPrefixTable[prefix]

proc toFactor*(prefix: SiPrefix): FloatType =
  ## note: can't compute value reasonably, due to hecto, centi, deci and deca
  case prefix
  of siQuecto:   result = 1e-30
  of siRonto:    result = 1e-27
  of siYocto:    result = 1e-24
  of siZepto:    result = 1e-21
  of siAtto:     result = 1e-18
  of siFemto:    result = 1e-15
  of siPico:     result = 1e-12
  of siNano:     result = 1e-9
  of siMicro:    result = 1e-6
  of siMilli:    result = 1e-3
  of siCenti:    result = 1e-2
  of siDeci:     result = 1e-1
  of siIdentity: result = 1e0
  of siDeca:     result = 1e1
  of siHecto:    result = 1e2
  of siKilo:     result = 1e3
  of siMega:     result = 1e6
  of siGiga:     result = 1e9
  of siTera:     result = 1e12
  of siPeta:     result = 1e15
  of siExa:      result = 1e18
  of siZetta:    result = 1e21
  of siYotta:    result = 1e24
  of siRonna:    result = 1e27
  of siQuetta:   result = 1e30
