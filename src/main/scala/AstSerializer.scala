object AstSerializer {
  private def exprToJson(expr: Expr): String = expr match {
    case Expr.Binary(left, op, right) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "BinOp", "elems": [${exprToJson(left)},${exprToJson(right)}]}"""

    case Expr.Unary(op, e) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "Unary", "elems": [${exprToJson(e)}]}"""

    case Expr.IntLiteral(_, token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "IntLiteral", "elems": []}"""

    case Expr.Ident(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Ident", "elems": []}"""

    case Expr.ErrorExpr(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Error", "elems": []}"""
  }

  private def stmtToJson(stmt: Stmt): String = stmt match {
    case Stmt.Decl(kw, ident, value) =>
      val identJson = s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Ident", "elems": []}"""
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Declare", "elems": [$identJson,${exprToJson(value)}]}"""

    case Stmt.Return(kw, value) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Return", "elems": [${exprToJson(value)}]}"""

    case Stmt.Assign(ident, value) =>
      val identJson = s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Ident", "elems": []}"""
      s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Assign", "elems": [$identJson,${exprToJson(value)}]}"""

    case Stmt.ExprStmt(expr) =>
      exprToJson(expr)

    case Stmt.ErrorStmt(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Error", "elems": []}"""
  }

  def toJson(stmts: List[Stmt]): String = {
    val (line, col) = ((stmt: Stmt) => (stmt.line, stmt.column))(stmts.head)
    val elemsJson = stmts.map(stmtToJson).mkString(",\n    ")

    s"""{
        "line": $line,
        "column": $col,
        "kind": "Program",
        "elems": [
        $elemsJson
        ]
        }"""
  }
}