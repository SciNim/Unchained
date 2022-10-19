# stdlib
import std / [macros, algorithm]

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

proc resolveTypeFromGenericInst(n: NimNode): NimNode =
  # simply leave as is
  result = n

proc getUnitTypeImpl*(n: NimNode): NimNode =
  case n.typeKind
  of ntyAlias: result = n.resolveTypeFromAlias()
  of ntyDistinct: result = n.resolveTypeFromDistinct()
  of ntyTypeDesc: result = n.resolveTypeFromTypeDesc()
  of ntyGenericInst: result = n.resolveTypeFromGenericInst()
  else: error("Unsupported : " & $n.typeKind)

proc getUnitType*(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkAccQuoted:
    var s: string
    for el in n:
      s.add el.strVal
    result = ident(s)
  else:
    result = n.getTypeInst.getUnitTypeImpl()

proc isUnitLessNumber*(n: NimNode): bool =
  case n.kind
  of nnkIdent: result = false # most likely direct unit
  of nnkAccQuoted:
    ## TODO: disallow things that are not a number?
    result = false
  else:
    ## NOTE: We cannot check for intLit / floatLit etc, as a `const` variable that
    ## has a unit will still be treated as a literal!
    let nTyp = n.getTypeInst
    if nTyp.kind == nnkSym:
      ## TODO: improve this check / include other numeric types
      if nTyp.strVal in ["float", "float64", "int", "int64"]:
        result = true

proc enumerateTypes*(t: NimNode): NimNode =
  ## Returns a bracket of all types described by the given
  ## "type class" `t`
  result = nnkBracket.newTree()
  for ch in t.getTypeImpl[1].getType:
    if ch.strVal == "or": continue
    result.add newLit(ch.strVal)

from std / strutils import normalize
proc resolveAlias*(n: NimNode): NimNode =
  ## returns the first type that is `distinct` (i.e. convert Newton -> KiloGram•Meter•Second⁻²)
  case n.kind
  of nnkDistinctTy: result = n
  of nnkBracketExpr:
    if n[0].strVal.normalize == "typedesc": ## Only resolve bracket expr if it's actually a `typedesc[X]`!
      if n[1].kind == nnkSym:
        result = n[1].getImpl.resolveAlias
      else:
        result = n[1].resolveAlias
    else:
      # else we leave the type as is, e.g. `seq[string]`
      result = n
  of nnkSym:
    if n.getTypeInst.kind != nnkSym: result = n.getTypeInst.resolveAlias
    elif n.getTypeImpl.kind != nnkSym: result = n.getTypeImpl.resolveAlias
    elif n.getImpl.kind != nnkSym: result = n.getImpl.resolveAlias
    else: result = n
  of nnkTypeDef:
    case n[2].kind
    of nnkDistinctTy: result = n[0]
    of nnkInfix: result = n[0]
    of nnkObjectTy: result = newEmptyNode()
    of nnkRefTy: result = newEmptyNode()
    of nnkPtrTy: result = newEmptyNode()
    else: result = n[2].getImpl.resolveAlias
  else: result = newEmptyNode()
