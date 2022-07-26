import std / [macros]

macro `^`*(x: untyped, num: static int): untyped =
  ## general purpose power using `^` for integers, which works for any
  ## type by rewriting to a product of `*`.
  result = nnkInfix.newTree(ident"*")

  proc addInfix(n, x: NimNode, num: int) =
    var it = n
    if num > 0:
      it.add nnkInfix.newTree(ident"*")
      it[1].addInfix(x, num - 1)
    while it.len < 3:
      it.add x

  result.addInfix(x, num - 2)
