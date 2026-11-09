object AstSerializer {
  private def exprToJson(expr: Expr): String = expr match {
    case Expr.Binary(left, op, right) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "BinOp", "elems": [${exprToJson(left)},${exprToJson(right)}]}"""

    case Expr.Unary(op, e) =>
      s"""{"line": ${op.line}, "column": ${op.column}, "kind": "Unary", "elems": [${exprToJson(e)}]}"""

    case Expr.IntLiteral(_, token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "IntLiteral", "elems": []}"""

    case Expr.BoolLiteral(_, token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "BoolLiteral", "elems": []}"""

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

    case Stmt.Assign(ident, assign, value) =>
      val identJson = s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Ident", "elems": []}"""
      s"""{"line": ${assign.line}, "column": ${assign.column}, "kind": "Assign", "elems": [$identJson,${exprToJson(value)}]}"""

    case Stmt.If(kw, cond, thenBranch, elseBranch) =>
      val elems = elseBranch match {
        case Some(elseStmt) => s"${exprToJson(cond)},${stmtToJson(thenBranch)},${stmtToJson(elseStmt)}"
        case None           => s"${exprToJson(cond)},${stmtToJson(thenBranch)}"
      }
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "If", "elems": [$elems]}"""

    case Stmt.While(kw, cond, body) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "While", "elems": [${exprToJson(cond)},${stmtToJson(body)}]}"""

    case Stmt.Break(kw) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Break", "elems": []}"""

    case Stmt.Continue(kw) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Continue", "elems": []}"""

    case Stmt.Block(lbrace, stmts) =>
      val elems = stmts.map(stmtToJson).mkString(",")
      s"""{"line": ${lbrace.line}, "column": ${lbrace.column}, "kind": "Block", "elems": [$elems]}"""

    case Stmt.ExprStmt(expr) =>
      exprToJson(expr)

    case Stmt.ErrorStmt(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Error", "elems": []}"""
  }

  def toJson(stmts: List[Stmt]): String = {
    val (line, col) = ((stmt: Stmt) => (stmt.line, stmt.column))(stmts.head)
    val elemsJson = stmts.map(stmtToJson).mkString(",\n")

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