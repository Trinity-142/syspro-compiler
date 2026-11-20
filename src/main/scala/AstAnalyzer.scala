object AstAnalyzer {
  case class AnalyzerState(
                            symbolTable: Map[String, Boolean] = Map.empty,  // variable name -> mutability
                            functions: Map[String, Int] = Map.empty,        // func name -> arity
                            errors: List[String] = Nil,
                            loopDepth: Int = 0
                          ) {
    def withError(msg: String): AnalyzerState = this.copy(errors = msg :: this.errors)
  }

  def analyzeProgram(decls: List[TopDecl]): AnalyzerState = {
    val stateWithFuncs = decls.foldLeft(AnalyzerState()) { (state, decl) =>
      decl match {
        case TopDecl.FuncDecl(_, ident, params, _) =>
          if (state.functions.contains(ident.lexeme))
            state.withError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' is already defined")
          else
            state.copy(functions = state.functions + (ident.lexeme -> params.length))

        case TopDecl.ExternDecl(_, ident, params) =>
          if (state.functions.contains(ident.lexeme))
            state.withError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' is already defined")
          else
            state.copy(functions = state.functions + (ident.lexeme -> params.length))

        case TopDecl.ErrorDecl(_) => state
      }
    }

    val hasMain = decls.exists {
      case TopDecl.FuncDecl(_, ident, _, _) => ident.lexeme == "main"
      case _                                => false
    }
    val stateAfterMainCheck =
      if (hasMain) stateWithFuncs
      else stateWithFuncs.withError("Semantic error: 'main' function must be explicitly defined")

    decls.foldLeft(stateAfterMainCheck)((state, decl) => analyzeDecl(state, decl))
  }

  def analyzeDecl(state: AnalyzerState, decl: TopDecl): AnalyzerState = decl match {
    case TopDecl.ExternDecl(_, paramsIdent, params) =>
      val (stateAfterParams, _) = params.foldLeft((state, Set.empty[String])) {
        case ((accState, seen), Expr.Param(pIdent)) =>
          if (seen.contains(pIdent.lexeme))
            (accState.withError(s"Semantic error at ${pIdent.line}:${pIdent.column}: parameter '${pIdent.lexeme}' is already defined"), seen)
          else
            (accState, seen + pIdent.lexeme)
      }
      stateAfterParams

    case TopDecl.FuncDecl(kw, ident, params, body) =>
      val (stateWithParams, paramSymbols) = params.foldLeft((state, Map.empty[String, Boolean])) {
        case ((accState, symAcc), Expr.Param(pIdent)) =>
          if (symAcc.contains(pIdent.lexeme))
            (accState.withError(s"Semantic error at ${pIdent.line}:${pIdent.column}: parameter '${pIdent.lexeme}' is already defined"), symAcc)
          else
            (accState, symAcc + (pIdent.lexeme -> true))
      }

      val funcInitialState = stateWithParams.copy(symbolTable = paramSymbols, loopDepth = 0)
      val stateAfterBody = analyze(funcInitialState, body)

      val stateAfterReturnCheck =
        if (hasReturnOnAllPaths(body)) stateAfterBody
        else stateAfterBody.withError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' must end with a 'return' statement on all paths")

      state.copy(errors = stateAfterReturnCheck.errors)

    case TopDecl.ErrorDecl(_) => state
  }

  def hasReturnOnAllPaths(stmt: Stmt): Boolean = stmt match {
    case _: Stmt.Return                              => true
    case Stmt.Block(_, stmts)                        => stmts.exists(hasReturnOnAllPaths)
    case Stmt.If(_, _, thenBranch, Some(elseBranch)) => hasReturnOnAllPaths(thenBranch) && hasReturnOnAllPaths(elseBranch)
    case _                                           => false
  }

  def analyze(state: AnalyzerState, stmt: Stmt | Expr): AnalyzerState = stmt match {

    // ------------ STATEMENTS ------------
    case Stmt.Decl(keyWord, ident, value) =>
      val stateAfterValue = analyze(state, value)

      if (!stateAfterValue.symbolTable.contains(ident.lexeme)) {
        val isMutable = keyWord.isInstanceOf[Token.Var]
        val updatedSymbolTable = stateAfterValue.symbolTable + (ident.lexeme -> isMutable)
        stateAfterValue.copy(symbolTable = updatedSymbolTable)
      } else {
        stateAfterValue.withError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' is already defined")
      }

    case Stmt.Assign(ident, _, value) =>
      val stateAfterIdent =
        if (!state.symbolTable.contains(ident.lexeme)) {
          state.withError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
        } else if (!state.symbolTable(ident.lexeme)) {
          state.withError(s"Semantic error at ${ident.line}:${ident.column}: reassignment to val '${ident.lexeme}'")
        } else {
          state
        }
      analyze(stateAfterIdent, value)

    case Stmt.If(_, cond, thenBranch, elseBranch) =>
      val stateAfterCond = analyze(state, cond)
      val stateAfterThen = analyze(stateAfterCond, thenBranch)
      val stateWithThenErrors = stateAfterCond.copy(errors = stateAfterThen.errors)

      elseBranch match {
        case Some(elseStmt) =>
          val stateAfterElse = analyze(stateWithThenErrors, elseStmt)
          stateWithThenErrors.copy(errors = stateAfterElse.errors)
        case None =>
          stateWithThenErrors
      }

    case Stmt.While(_, cond, body) =>
      val stateAfterCond = analyze(state, cond)
      val stateAfterBody = analyze(stateAfterCond.copy(loopDepth = stateAfterCond.loopDepth + 1), body)
      stateAfterCond.copy(errors = stateAfterBody.errors)

    case Stmt.Break(kw) =>
      if (state.loopDepth == 0) {
        state.withError(s"Semantic error at ${kw.line}:${kw.column}: 'break' outside of loop")
      } else state

    case Stmt.Continue(kw) =>
      if (state.loopDepth == 0) {
        state.withError(s"Semantic error at ${kw.line}:${kw.column}: 'continue' outside of loop")
      } else state

    case Stmt.Block(_, stmts) =>
      val stateAfterBlock = stmts.foldLeft(state)((acc, s) => analyze(acc, s))
      state.copy(errors = stateAfterBlock.errors)

    case Stmt.Return(_, expr) => analyze(state, expr)
    case Stmt.ExprStmt(expr) => analyze(state, expr)
    case Stmt.ErrorStmt(_) => state

    // ------------ EXPRESSIONS ------------
    case Expr.Ident(ident) =>
      if (!state.symbolTable.contains(ident.lexeme)) {
        state.withError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
      } else state

    case Expr.Call(ident, args) =>
      val stateAfterCallee = state.functions.get(ident.lexeme) match {
        case None =>
          state.withError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' not found")
        case Some(expectedArity) if expectedArity != args.length =>
          state.withError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' expects $expectedArity arguments, but ${args.length} given")
        case Some(_) =>
          state
      }
      args.foldLeft(stateAfterCallee)((acc, arg) => analyze(acc, arg))

    case Expr.Param(_) => state

    case Expr.Binary(left, _, right) =>
      analyze(analyze(state, left), right)

    case Expr.Unary(_, expr) =>
      analyze(state, expr)

    case _: (Expr.IntLiteral | Expr.BoolLiteral | Expr.ErrorExpr) =>
      state
  }
}
