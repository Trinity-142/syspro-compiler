case class AnalyzerState(symbolTable: Map[String, Boolean], semanticErrors: List[String]) {
  def withError(msg: String): AnalyzerState = this.copy(semanticErrors = msg :: this.semanticErrors)
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

  case Stmt.Assign(ident, value) =>
    val stateAfterIdent =
      if (!state.symbolTable.contains(ident.lexeme)) {
        state.withError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
      } else if (!state.symbolTable(ident.lexeme)) {
        state.withError(s"Semantic error at ${ident.line}:${ident.column}: reassignment to val '${ident.lexeme}'")
      } else {
        state
      }
    analyze(stateAfterIdent, value)

  case Stmt.Return(_, expr) => analyze(state, expr)
  case Stmt.ExprStmt(expr)  => analyze(state, expr)
  case Stmt.ErrorStmt(_)    => state

  // ------------ EXPRESSIONS ------------

  case Expr.Ident(ident) =>
    if (!state.symbolTable.contains(ident.lexeme)) {
      state.withError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
    } else state

  case Expr.Binary(left, _, right) =>
    analyze(analyze(state, left), right)

  case Expr.Unary(_, expr) =>
    analyze(state, expr)

  case _: (Expr.IntLiteral | Expr.ErrorExpr) =>
    state
}
