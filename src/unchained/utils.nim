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

proc resolveTypeFromAlias(n: NimNode): NimNode =
  case n.kind
  of nnkSym:
    let typ = n.getImpl
    doAssert typ.kind == nnkTypeDef, " no, was " & $typ.treerepr
    if typ[2].typeKind == ntyAlias:
      result = typ[2].resolveTypeFromAlias()
    else:
      case typ[2].kind
      of nnkInfix: # this is a type class A = B | C | D, ... return input
        result = n
      else:
        result = typ[2]
  else:
    let typ = n.getTypeImpl
    doAssert typ.kind == nnkDistinctTy, " no, was " & $typ.treerepr
    result = typ[0]

proc resolveTypeFromDistinct(n: NimNode): NimNode =
  let typ = n.getImpl
  doAssert typ.kind == nnkTypeDef
  result = typ[0]

proc resolveTypeFromTypeDesc(n: NimNode): NimNode =
  let typ = n.getType
  doAssert typ.kind == nnkBracketExpr, "no, was " & $typ.treerepr
  result = typ[1]

proc getUnitTypeImpl*(n: NimNode): NimNode =
  case n.typeKind
  of ntyAlias: result = n.resolveTypeFromAlias()
  of ntyDistinct: result = n.resolveTypeFromDistinct()
  of ntyTypeDesc: result = n.resolveTypeFromTypeDesc()
  else: error("Unsupported : " & $n.typeKind)

proc getUnitType*(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkAccQuoted:
    var s: string
    for el in n:
      s.add el.strVal
    result = ident(s)
  else: result = n.getTypeInst.getUnitTypeImpl()

proc isUnitLessNumber*(n: NimNode): bool =
  case n.kind
  of nnkIntLit .. nnkFloatLit: result = true
  of nnkIdent: result = false # most likely direct unit
  of nnkAccQuoted:
    ## TODO: disallow things that are not a number?
    result = false
  else:
    let nTyp = n.getTypeInst
    if nTyp.kind == nnkSym:
      ## TODO: improve this check / include other numeric types
      if nTyp.strVal in ["float", "float64", "int", "int64"]:
        result = true
