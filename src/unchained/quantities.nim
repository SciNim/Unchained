import std / [macros, sets, sequtils, tables, tables]

import macro_utils

type
  QuantityType* = enum
    qtFundamental, qtCompound

  ## TODO: add `id` to quantities to better map them to dimensions such that
  ## we can avoid performing string operations the whole time?
  CTBaseQuantity* = object
    name*: string

  QuantityPower* = object
    quant*: CTBaseQuantity
    power*: int

  ## A quantity can either be a base quantity or a compound consisting of multiple
  ## CTBaseQuantities of different powers.
  CTQuantity* = object
    case kind*: QuantityType
    of qtFundamental: b*: CTBaseQuantity
    of qtCompound:
      name*: string # name of the derived quantity (e.g. Force)
      baseSeq*: seq[QuantityPower]


proc `==`*(q1, q2: CTBaseQuantity): bool = q1.name == q2.name
proc `<`*(q1, q2: CTBaseQuantity): bool = q1.name < q2.name

proc `<`*(q1, q2: QuantityPower): bool =
  if q1.quant < q2.quant:
    result = true
  elif q1.quant > q2.quant:
    result = false
  else: # same quantity
    result = q1.power < q2.power

proc `$`*(q: CTBaseQuantity): string = q.name

proc `$`*(q: CTQuantity): string =
  ## XXX: add full option to also get baseSeq
  result = "(CTQuantity: "
  case q.kind
  of qtFundamental: result.add $q.b
  of qtCompound: result.add q.name
  result.add ")"

#const QuantityTab = CacheTable"QuantityTab"
var QuantityTab* {.compiletime.} = initTable[string, CTQuantity]()

iterator compoundQuantities*(qt: Table[string, CTQuantity]): CTQuantity =
  ## Yields all units that corresond to pure compound quantities (i.e. those
  ## that have no `conversion` attached).
  for name, quant in pairs(qt):
    if quant.kind == qtCompound:
      yield quant

proc toQuantityPower*(q: CTQuantity): seq[QuantityPower] =
  ## Turns the given quantity into a `seq[QuantityPower]` independent of
  ## the kind of quantity. Useful to perform dimensional analysis.
  case q.kind
  of qtFundamental: result.add QuantityPower(quant: q.b, power: 1)
  of qtCompound: result = q.baseSeq

import std / hashes
proc hash*(q: CTBaseQuantity): Hash =
  result = result !& hash(q.name)
  result = !$result

proc hash*(qp: QuantityPower): Hash =
  result = result !& hash(qp.quant)
  result = result !& hash(qp.power)
  result = !$result

proc hash*(q: CTQuantity): Hash =
  result = result !& hash(q.kind)
  case q.kind
  of qtFundamental:
    result = result !& hash(q.b)
  of qtCompound:
    result = result !& hash(q.name)
    result = result !& hash(q.baseSeq)
  result = !$result

proc `==`*(q1, q2: CTQuantity): bool =
  if q1.kind == q2.kind:
    case q1.kind
    of qtFundamental: result = q1.b == q2.b
    of qtCompound: result = q1.name == q2.name and
      q1.baseSeq == q2.baseSeq
  else:
    result = false

proc contains*(s: HashSet[CTBaseQuantity], key: string): bool =
  result = CTBaseQuantity(name: key) in s

proc getName*(q: CTQuantity): string =
  case q.kind
  of qtFundamental: result = q.b.name
  of qtCompound: result = q.name

proc reduce(s: seq[QuantityPower]): seq[QuantityPower] =
  ## Reduces possible duplicate units with different powers.
  var tab = initTable[string, int]()
  for q in s:
    let name = q.quant.name
    if name in tab:
      tab[name] += q.power
    else:
      tab[name] = q.power
  for k, v in pairs(tab):
    if v != 0:
      result.add QuantityPower(
        quant: CTBaseQuantity(name: k),
        power: v
      )

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
      let qt = CTQuantity(kind: qtFundamental, b: CTBaseQuantity(name: quant.strVal))
      result.add qt
      QuantityTab[qt.b.name] = qt
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
      var qt = CTQuantity(kind: qtCompound, name: quant[0].strVal)
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
      QuantityTab[qt.name] = qt
    else:
      error("Invalid node kind " & $quant.kind & " in `Derived:` for description of derived quantities.")

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
            of qtFundamental: "Quantity"
            of qtCompound:
              if quant.baseSeq.reduce.len > 0:
                "CompoundQuantity"
              else:
                "UnitLess"
    let qName = quant.getName()
    result.add defineDistinctType(qName, q)
    quantList.add ident(qName)
  let qtc = case qType
            of qtFundamental: exportIt("BaseQuantity")
            of qtCompound: exportIt("DerivedQuantity")
  result.add nnkTypeDef.newTree(qtc, newEmptyNode(), genTypeClass(quantList))

proc genQuantityKindEnum*(base, derived: seq[CTQuantity]): NimNode =
  result = nnkTypeDef.newTree(exportIt("QuantityKind"), newEmptyNode())
  var en = nnkEnumTy.newTree(
    newEmptyNode(),
    ident"qkUnitLess" # UnitLess quantity kind must be first element
  )
  for b in base:
    let bName = b.getName()
    en.add nnkEnumFieldDef.newTree(ident("qk" & bName), newLit bName)
  for d in derived:
    let dName = d.getName()
    en.add nnkEnumFieldDef.newTree(ident("qk" & dName), newLit dName)
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
  result.add genQuantityTypes(baseQuantities, qtFundamental)
  result.add genQuantityTypes(derivedQuants, qtCompound)
  result.add genQuantityKindEnum(baseQuantities, derivedQuants)
  result.add nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      exportIt("SomeQuantity"),
      newEmptyNode(),
      nnkInfix.newTree(ident"|", ident"BaseQuantity", ident"DerivedQuantity")
    )
  )
