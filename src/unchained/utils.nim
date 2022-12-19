import std / [macros]
from core_types import FloatType

macro `^`*(x: typed, num: static int): untyped =
  ## general purpose power using `^` for integers, which works for any
  ## type by rewriting to a product of `*`.
  ##
  ## For the special cases of -1, 0, 1 we simply rewrite to the correct
  ## result. Negative powers are written as `1 / x^p`
  if num == 0:
    result = quote do:
      1.0
  elif num == 1:
    result = x
  elif num == -1:
    ## Assume that the type supports addition by a FloatType!
    result = quote do:
      1.0 / `x`
  else:
    result = nnkInfix.newTree(ident"*")

    proc addInfix(n, x: NimNode, num: int) =
      var it = n
      if num > 0:
        it.add nnkInfix.newTree(ident"*")
        it[1].addInfix(x, num - 1)
      while it.len < 3:
        it.add x

    result.addInfix(x, abs(num) - 2)

    # invert if num is negative
    if num < -1:
      ## Assume that the type supports addition by a FloatType!
      result = quote do:
        1.0 / (`result`)


## Number of digits to use as epsilon for unit comparison in `almostEqual`
const UnitCompareEpsilon {.intdefine.} = 8
import std / fenv
proc almostEqual*(a, b: FloatType, epsilon = 10^(-UnitCompareEpsilon)): bool =
  ## Comparison of two floats, taken from `datamancer` implementation. Only used
  ## internally to compare two units.
  # taken from
  # https://floating-point-gui.de/errors/comparison/
  let
    absA = abs(a)
    absB = abs(b)
    diff = abs(a - b)
  if a == b: # shortcut, handles infinities
    result = true
  elif a == 0 or b == 0 or (absA + absB) < minimumPositiveValue(FloatType):
    # a or b is zero or both are extremely close to it
    # relative error is less meaningful here
    result = diff < (epsilon * minimumPositiveValue(FloatType))
  else:
    # use relative error
    result = diff / min(absA + absB, maximumPositiveValue(FloatType)) < epsilon
