import std / strutils
import cligen/argcvt
import unchained
proc argParse*[T: SomeUnit](dst: var T, dfl: T,
                           a: var ArgcvtParams): bool =
  try:
    let aStr = a.val
    var tmp: T
    let unitName = "." & unitOf(tmp)
    if aStr.endsWith(unitName):
      proc removeSuffix(s, p: string): string =
        result = s
        result.removeSuffix(p)
      dst = aStr.removeSuffix(unitName).parseFloat.T
    else:
      dst = aStr.strip.parseFloat.T
    result = true
  except:
    result = false

proc argHelp*[T: SomeUnit](dfl: T; a: var ArgcvtParams): seq[string] =
  var tmp: T
  result = @[ a.argKeys, unitOf(tmp), $dfl ]
