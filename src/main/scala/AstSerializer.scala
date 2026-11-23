object AstSerializer {
  private def identToJson(ident: Token): String =
    s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Ident", "elems": []}"""

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
      identToJson(token)

    case Expr.Param(ident) =>
      identToJson(ident)

    case Expr.Call(ident, args) =>
      val elems = (identToJson(ident) :: args.map(exprToJson)).mkString(",")
      s"""{"line": ${ident.line}, "column": ${ident.column}, "kind": "Call", "elems": [$elems]}"""

    case Expr.ErrorExpr(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Error", "elems": []}"""
  }

  private def stmtToJson(stmt: Stmt): String = stmt match {
    case Stmt.Decl(kw, ident, value) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Declare", "elems": [${identToJson(ident)},${exprToJson(value)}]}"""

    case Stmt.Return(kw, value) =>
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "Return", "elems": [${exprToJson(value)}]}"""

    case Stmt.Assign(ident, assign, value) =>
      s"""{"line": ${assign.line}, "column": ${assign.column}, "kind": "Assign", "elems": [${identToJson(ident)},${exprToJson(value)}]}"""

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

  private def topDeclToJson(decl: TopDecl): String = decl match {
    case TopDecl.FuncDecl(kw, ident, params, body) =>
      val elems = (identToJson(ident) :: params.map(exprToJson) ::: List(stmtToJson(body))).mkString(",")
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "FuncDecl", "elems": [$elems]}"""

    case TopDecl.ExternDecl(kw, ident, params) =>
      val elems = (identToJson(ident) :: params.map(exprToJson)).mkString(",")
      s"""{"line": ${kw.line}, "column": ${kw.column}, "kind": "ExternDecl", "elems": [$elems]}"""

    case TopDecl.ErrorDecl(token) =>
      s"""{"line": ${token.line}, "column": ${token.column}, "kind": "Error", "elems": []}"""
  }

  def toJson(decls: List[TopDecl]): String = {
    val (line, col) = decls.headOption.map(d => (d.line, d.column)).getOrElse((1, 1))
    val elemsJson = decls.map(topDeclToJson).mkString(",\n")

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
