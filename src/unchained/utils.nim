import std / [macros]

macro `^`*(x: typed, num: static int): untyped =
  ## general purpose power using `^` for integers, which works for any
  ## type by rewriting to a product of `*`.
  ##
  ## For the special cases of -1, 0, 1 we simply rewrite to the correct
  ## result. Negative powers are written as `1 / x^p`
  if num == 0:
    result = quote do:
      `x` * typeof(`x`)(0.0)
  elif num == 1:
    result = x
  elif num == -1:
    ## Assume that the type supports addition by a float!
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
      ## Assume that the type supports addition by a float!
      result = quote do:
        1.0 / (`result`)
