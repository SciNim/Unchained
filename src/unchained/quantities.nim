import std / [macros, sets, sequtils]

import utils

type
  QuantityType* = enum
    qtBase, qtDerived

  CTBaseQuantity* = object
    name*: string

  QuantityPower* = object
    quant*: CTBaseQuantity
    power*: int

  ## A quantity can either be a base quantity or a compound consisting of multiple
  ## CTBaseQuantities of different powers.
  CTQuantity* = object
    case kind*: QuantityType
    of qtBase: b*: CTBaseQuantity
    of qtDerived:
      name*: string # name of the derived quantity (e.g. Force)
      baseSeq*: seq[QuantityPower]

proc `==`*(q1, q2: CTQuantity): bool =
  if q1.kind == q2.kind:
    case q1.kind
    of qtBase: result = q1.b == q2.b
    of qtDerived: result = q1.name == q2.name and
      q1.baseSeq == q2.baseSeq
  else:
    result = false

proc contains*(s: HashSet[CTBaseQuantity], key: string): bool =
  result = CTBaseQuantity(name: key) in s

proc getName*(q: CTQuantity): string =
  case q.kind
  of qtBase: result = q.b.name
  of qtDerived: result = q.name

proc parseBaseQuantities*(quants: NimNode): seq[CTQuantity] =
  ## Parses the given quantities
  ##
  ## Given:
  ##
  ##  Base:
  ##    Time
  ##    Length
  ##    ...
  ##
  ## As Nim AST:
  ##    Call
  ##    Ident "Base"
  ##    StmtList
  ##      Ident "Time"
  ##      Ident "Length"
  ##    ...
  ##
  ## into corresponding `CTQuantity` objects.
  doAssert quants.len == 2
  doAssert quants[0].kind == nnkIdent and quants[0].strVal == "Base"
  doAssert quants[1].kind == nnkStmtList
  for quant in quants[1]:
    case quant.kind
    of nnkIdent:
      # simple base quantity
      result.add CTQuantity(kind: qtBase, b: CTBaseQuantity(name: quant.strVal))
    else:
      error("Invalid node kind " & $quant.kind & " in `Base:` for description of base quantities.")

proc parseDerivedQuantities*(quants: NimNode, baseQuantities: HashSet[CTBaseQuantity]): seq[CTQuantity] =
  ## Parses the given derived quantities
  ##
  ## Given:
  ##
  ##  Derived:
  ##    Frequency = (Time, -1)
  ##    ...
  ##
  ## As Nim AST:
  ##    Call
  ##    Ident "Derived"
  ##    StmtList
  ##      Call
  ##        Ident "Acceleration"
  ##        StmtList
  ##          Bracket
  ##            TupleConstr
  ##              Ident "Mass"
  ##              IntLit 1
  ##            TupleConstr
  ##              Ident "Speed"
  ##              IntLit -2
  ##     ...
  ##
  ##
  ## into corresponding `CTQuantity` objects.
  doAssert quants.len == 2
  doAssert quants[0].kind == nnkIdent and quants[0].strVal == "Derived"
  doAssert quants[1].kind == nnkStmtList
  for quant in quants[1]:
    case quant.kind
    of nnkCall:
      doAssert quant.len == 2
      doAssert quant[0].kind == nnkIdent
      doAssert quant[1].kind == nnkStmtList
      doAssert quant[1][0].kind == nnkBracket
      var qt = CTQuantity(kind: qtDerived, name: quant[0].strVal)
      for tup in quant[1][0]:
        case tup.kind
        of nnkTupleConstr:
          doAssert tup[0].kind == nnkIdent and tup[1].kind == nnkIntLit
          let base = tup[0].strVal
          let power = tup[1].intVal.int
          if base notin baseQuantities:
            error("Given base quantitiy `" & $base & "` is unknown! Make sure to define " &
              "it in the `Base:` block.")
          qt.baseSeq.add QuantityPower(quant: CTBaseQuantity(name: base), power: power)
        else:
          error("Invalid node kind " & $tup.kind & " in dimensional argument to derived quantity " &
            $quant.repr & ".")
      result.add qt
    else:
      error("Invalid node kind " & $quant.kind & " in `Derived:` for description of derived quantities.")

proc exportIt(n: string): NimNode =
  result = nnkPostfix.newTree(ident"*", ident(n))

proc genQuantityTypes*(quants: seq[CTQuantity], qType: QuantityType): NimNode =
  ## Generates the base quantities based on the given list of quantities
  ##
  ##  type
  ##    Time* = distinct Quantity
  ##    Length* = distinct Quantity
  ##    ...
  ##
  ##    BaseQuantity* = Time | Length | ...
  var quantList = newSeq[NimNode]()
  result = nnkTypeSection.newTree()
  for quant in quants:
    doAssert quant.kind == qType
    let q = case quant.kind
            of qtBase: ident"Quantity"
            of qtDerived: ident"CompoundQuantity"
    let qName = quant.getName()
    result.add nnkTypeDef.newTree(
      exportIt(qName),
      newEmptyNode(),
      nnkDistinctTy.newTree(q)
    )
    quantList.add ident(qName)
  let qtc = case qType
            of qtBase: exportIt("BaseQuantity")
            of qtDerived: exportIt("DerivedQuantity")
  result.add nnkTypeDef.newTree(qtc, newEmptyNode(), genTypeClass(quantList))

proc genQuantityKindEnum*(base, derived: seq[CTQuantity]): NimNode =
  result = nnkTypeDef.newTree(exportIt("QuantityKind"), newEmptyNode())
  var en = nnkEnumTy.newTree(
    newEmptyNode(),
    ident"qkUnitLess" # UnitLess quantity kind must be first element
  )
  for b in base:
    en.add ident("qk" & b.getName())
  for d in derived:
    en.add ident("qk" & d.getName())
  result.add en
  result = nnkTypeSection.newTree(result)

macro declareQuantities*(typs: untyped): untyped =
  var baseQuant = nnkTypeDef.newTree(ident"BaseQuantity")
  result = newStmtList()
  var baseQuantities: seq[CTQuantity]
  var derivedQuants: seq[CTQuantity]
  for typ in typs:
    if typ.kind != nnkCall:
      error("Invalid node kind " & $typ.kind & " in declaration of quantities.")
    if typ[0].strVal == "Base":
      # defines the base quantities
      baseQuantities = parseBaseQuantities(typ)
    elif typ[0].strVal == "Derived":
      # defines derived quantities
      if baseQuantities.len == 0:
        error("`Base:` block to define base quantities must come before `Derived:` block.")
      derivedQuants = parseDerivedQuantities(typ, baseQuantities.mapIt(it.b).toHashSet())
    else:
      error("Invalid type of quantities: " & $typ.repr)
  result.add genQuantityTypes(baseQuantities, qtBase)
  result.add genQuantityTypes(derivedQuants, qtDerived)
  result.add genQuantityKindEnum(baseQuantities, derivedQuants)
  result.add nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      exportIt("SomeQuantity"),
      newEmptyNode(),
      nnkInfix.newTree(ident"|", ident"BaseQuantity", ident"DerivedQuantity")
    )
  )
