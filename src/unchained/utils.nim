import std / [macros, algorithm]

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

proc genTypeClass*(e: var seq[NimNode]): NimNode =
  ## Helper to generate a "type class" (using `|`) of multiple
  ## types, because for _reasons_ the Nim AST for that is nested infix
  ## calls apparently.
  if e.len == 2:
    result = nnkInfix.newTree(ident"|", e[0], e[1])
  else:
    let el = e.pop
    result = nnkInfix.newTree(ident"|",
                              genTypeClass(e),
                              el)

proc exportIt*(n: string): NimNode =
  result = nnkPostfix.newTree(ident"*", ident(n))

proc defineDistinctType*(name, rhs: string): NimNode =
  result = nnkTypeDef.newTree(
      exportIt(name),
      newEmptyNode(),
      nnkDistinctTy.newTree(ident(rhs))
  )

proc defineType*(name, rhs: string): NimNode =
  result = nnkTypeDef.newTree(
      exportIt(name),
      newEmptyNode(),
      ident(rhs)
  )

iterator getPow10Digits*(x: int): int =
  ## yields all digits in given integer
  var digits: seq[int]
  var val = abs(x)
  while val > 0:
    digits.add val mod 10
    val = val div 10
  for el in digits.reversed:
    yield el
