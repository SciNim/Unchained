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
  of ntyFloat32, ntyFloat64, ntyFloat: result = n
  of ntyUserTypeClass:
    ## NOTE: Attempting to resolve a type from such an implicit generic doesn't
    ## work properly. See tests/tResolveImplicitQuantity.nim
    ## for a case in which no `getType*` yields anything useful.
    error("""Cannot get a type from a `ntyUserTypeClass`. You are likely using a quantity concept as a generic argument directly, instead of using an explicit generic. Replace usage of the kind

    ```nim
    proc foo(x: Length)
    ```

    by

    ```nim
    proc foo[T: Length](x: T)
    ```

    as currently we cannot reliably extract the real type of `Length` in the former case.
    If your problem is a different one, please open an issue at:
    https://github.com/SciNim/unchained
""")
  else: error("Unsupported : " & $n.typeKind)

proc toStrUnit*(n: NimNode): string =
  doAssert n.kind == nnkAccQuoted
  result = newStringOfCap(100)
  for el in n:
    result.add el.strVal

proc getUnitType*(n: NimNode): NimNode =
  case n.kind
  of nnkIdent: result = n
  of nnkAccQuoted:
    result = ident(n.toStrUnit())
  else:
    result = n.getTypeInst.getUnitTypeImpl()

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
