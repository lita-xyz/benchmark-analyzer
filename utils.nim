import std / macros
proc replaceNodes(ast: NimNode, what: NimNode, by: NimNode): NimNode =
  # Replace "what" ident node by "by"
  proc inspect(node: NimNode): NimNode =
    case node.kind:
    of {nnkIdent, nnkSym}:
      if node.eqIdent(what): return by
      return node
    else:
      if node.len == 0: return node
      result = node.kind.newTree()
      for child in node:
        result.add inspect(child)
  result = inspect(ast)

macro staticFor*(id, strVal: untyped{ident}, theEnum: typed, body: untyped): untyped =
  ## A basic `staticFor` which unrolls a loop over an enum so that we have
  ## access to the string value of each element as a CT constant.
  let impl = theEnum.getImpl
  # EnumTy
  #   Empty
  #   EnumFieldDef
  #     Sym "bkRecursiveFibonacci"
  #     StrLit "Fib$i Rec"
  #   EnumFieldDef
  #     Sym "bkIterFibonacci"
  #     StrLit "Fib$i Iter"
  #  ....
  result = newStmtList()
  for el in impl[2]: # 2 is the `EnumTy`
    if el.kind == nnkEmpty: continue
    doAssert el.kind == nnkEnumFieldDef, "Unexpected found: " & $el.treerepr
    result.add nnkBlockStmt.newTree(
      ident("unrolledIter_" & $el[0]),
      body.replaceNodes(id, el[0]).replaceNodes(strVal, el[1]))
