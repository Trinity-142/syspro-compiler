object AstSerializer {
  
  private def getExprCoords(expr: Expr): (Int, Int) = expr match {
    case Expr.Binary(_, op, _)     => (op.line, op.column)
    case Expr.Unary(op, _)         => (op.line, op.column)
    case Expr.IntLiteral(_, token) => (token.line, token.column)
    case Expr.Ident(token)         => (token.line, token.column)
    case Expr.ErrorExpr()          => (0, 0)
  }

  private def getStmtCoords(stmt: Stmt): (Int, Int) = stmt match {
    case Stmt.Decl(kw, _, _)    => (kw.line, kw.column)
    case Stmt.Return(kw, _)     => (kw.line, kw.column)
    case Stmt.Assign(ident, _)  => (ident.line, ident.column)
    case Stmt.ExprStmt(expr)    => getExprCoords(expr)
    case Stmt.ErrorStmt()       => (0, 0)
  }

  private def exprToJson(expr: Expr): String = expr match {
    case Expr.Binary(left, op, right) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "BinOp", "left": ${exprToJson(left)}, "right": ${exprToJson(right)}}"""
    case Expr.Unary(op, e) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "Unary", "operand": ${exprToJson(e)}}"""
    case Expr.IntLiteral(_, token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "IntLiteral"}"""
    case Expr.Ident(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Ident"}"""
    case Expr.ErrorExpr() =>
      s"""{"kind": "Error"}"""
  }

  private def stmtToJson(stmt: Stmt): String = stmt match {
    case Stmt.Decl(kw, _, value) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Declare", "init": ${exprToJson(value)}}"""
    case Stmt.Return(kw, value) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Return", "value": ${exprToJson(value)}}"""
    case Stmt.Assign(ident, value) =>
      s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Assign", "target": {"line": ${ident.line}, "column": ${ident.column}, "kind": "Ident"}, "value": ${exprToJson(value)}}"""
    case Stmt.ExprStmt(expr) =>
      val (line, col) = getExprCoords(expr)
      s"""{"line": $line, "column": $col, "kind": "ExprStmt", "value": ${exprToJson(expr)}}"""
    case Stmt.ErrorStmt() =>
      s"""{"kind": "ErrorStmt"}"""
  }

  def toJson(stmts: List[Stmt]): String = {
    val (line, col) = stmts.headOption.map(getStmtCoords).getOrElse((1, 1))
    val bodyJson = stmts.map(stmtToJson).mkString(",\n    ")

    s"""{
  "line": $line,
  "column": $col,
  "kind": "Program",
  "body": [
    $bodyJson
  ]
}"""
  }
}
