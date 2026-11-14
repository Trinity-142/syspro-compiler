object AstAnalyzer {
  case class AnalyzerState(
    symbolTable: Map[String, Boolean] = Map.empty,
    errors: List[String] = Nil,
    loopDepth: Int = 0) {
    def withError(msg: String): AnalyzerState = this.copy(errors = msg :: this.errors)
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

    case Expr.Binary(left, _, right) =>
      analyze(analyze(state, left), right)

    case Expr.Unary(_, expr) =>
      analyze(state, expr)

    case _: (Expr.IntLiteral | Expr.BoolLiteral | Expr.ErrorExpr) =>
      state
  }
}
