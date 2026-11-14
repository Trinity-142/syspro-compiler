trait Coords {
  def line: Int
  def column: Int
}

enum Stmt(val startToken: Token) extends Coords {
  def line: Int = startToken.line
  def column: Int = startToken.column

  case Return(keyWord: Token, expr: Expr)                                         extends Stmt(keyWord)
  case Decl(keyWord: Token, ident: Token, value: Expr)                            extends Stmt(keyWord)
  case Assign(ident: Token, assign: Token.Assign, value: Expr)                    extends Stmt(ident)
  case ExprStmt(expr: Expr)                                                       extends Stmt(expr.startToken)
  case If(keyWord: Token, cond: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt(keyWord)
  case While(keyWord: Token, cond: Expr, body: Stmt)                              extends Stmt(keyWord)
  case Break(keyWord: Token)                                                      extends Stmt(keyWord)
  case Continue(keyWord: Token)                                                   extends Stmt(keyWord)
  case Block(lbrace: Token, stmts: List[Stmt])                                    extends Stmt(lbrace)
  case ErrorStmt(token: Token)                                                    extends Stmt(token)
}

enum Expr(val startToken: Token) extends Coords {
  def line: Int = startToken.line
  def column: Int = startToken.column

  case Binary(left: Expr, op: Token, right: Expr) extends Expr(op)
  case Unary(op: Token, expr: Expr)               extends Expr(op)
  case IntLiteral(value: Long, token: Token)      extends Expr(token)
  case BoolLiteral(value: Boolean, token: Token)  extends Expr(token)
  case Ident(ident: Token)                        extends Expr(ident)
  case ErrorExpr(token: Token)                    extends Expr(token)
}

enum Token(val line: Int, val column: Int) extends Coords {
  case Rparen(l: Int, c: Int)   extends Token(l, c)
  case Lparen(l: Int, c: Int)   extends Token(l, c)
  case Lbrace(l: Int, c: Int)   extends Token(l, c)
  case Rbrace(l: Int, c: Int)   extends Token(l, c)
  case Assign(l: Int, c: Int)   extends Token(l, c)
  case Semi(l: Int, c: Int)     extends Token(l, c)
  case Plus(l: Int, c: Int)     extends Token(l, c)
  case Minus(l: Int, c: Int)    extends Token(l, c)
  case Mult(l: Int, c: Int)     extends Token(l, c)
  case Div(l: Int, c: Int)      extends Token(l, c)
  case Eq(l: Int, c: Int)       extends Token(l, c)
  case Ne(l: Int, c: Int)       extends Token(l, c)
  case Lt(l: Int, c: Int)       extends Token(l, c)
  case Gt(l: Int, c: Int)       extends Token(l, c)
  case Le(l: Int, c: Int)       extends Token(l, c)
  case Ge(l: Int, c: Int)       extends Token(l, c)
  case And(l: Int, c: Int)      extends Token(l, c)
  case Or(l: Int, c: Int)       extends Token(l, c)
  case Not(l: Int, c: Int)      extends Token(l, c)

  case Val(l: Int, c: Int)      extends Token(l, c)
  case Var(l: Int, c: Int)      extends Token(l, c)
  case Return(l: Int, c: Int)   extends Token(l, c)
  case If(l: Int, c: Int)       extends Token(l, c)
  case Else(l: Int, c: Int)     extends Token(l, c)
  case While(l: Int, c: Int)    extends Token(l, c)
  case Break(l: Int, c: Int)    extends Token(l, c)
  case Continue(l: Int, c: Int) extends Token(l, c)
  case True(l: Int, c: Int)     extends Token(l, c)
  case False(l: Int, c: Int)    extends Token(l, c)
  case Eof(l: Int, c: Int)      extends Token(l, c)

  case Ident(value: String, l: Int, c: Int)  extends Token(l, c)
  case IntTok(value: String, l: Int, c: Int) extends Token(l, c)
  case ErrorTok(msg: String, l: Int, c: Int) extends Token(l, c)

  def lexeme: String = this match {
    case _: Rparen       => ")"
    case _: Lparen       => "("
    case _: Lbrace       => "{"
    case _: Rbrace       => "}"
    case _: Assign       => "="
    case _: Semi         => ";"
    case _: Plus         => "+"
    case _: Minus        => "-"
    case _: Mult         => "*"
    case _: Div          => "/"
    case _: Eq           => "=="
    case _: Ne           => "!="
    case _: Lt           => "<"
    case _: Gt           => ">"
    case _: Le           => "<="
    case _: Ge           => ">="
    case _: And          => "&&"
    case _: Or           => "||"
    case _: Not          => "!"
    case _: Val          => "val"
    case _: Var          => "var"
    case _: Return       => "return"
    case _: If           => "if"
    case _: Else         => "else"
    case _: While        => "while"
    case _: Break        => "break"
    case _: Continue     => "continue"
    case _: True         => "true"
    case _: False        => "false"
    case Ident(v, _, _)  => v
    case IntTok(v, _, _) => v
    case ErrorTok(msg, _, _) => msg
    case _: Eof          => "EOF"
  }

  def kind: String = this match {
    case _: Rparen   => "RPAREN"
    case _: Lparen   => "LPAREN"
    case _: Lbrace   => "LBRACE"
    case _: Rbrace   => "RBRACE"
    case _: Assign   => "ASSIGN"
    case _: Semi     => "SEMI"
    case _: Plus     => "PLUS"
    case _: Minus    => "MINUS"
    case _: Mult     => "MULT"
    case _: Div      => "DIV"
    case _: Eq       => "EQ"
    case _: Ne       => "NE"
    case _: Lt       => "LT"
    case _: Gt       => "GT"
    case _: Le       => "LE"
    case _: Ge       => "GE"
    case _: And      => "AND"
    case _: Or       => "OR"
    case _: Not      => "NOT"
    case _: Val      => "VAL"
    case _: Var      => "VAR"
    case _: Return   => "RETURN"
    case _: If       => "IF"
    case _: Else     => "ELSE"
    case _: While    => "WHILE"
    case _: Break    => "BREAK"
    case _: Continue => "CONTINUE"
    case _: True     => "TRUE"
    case _: False    => "FALSE"
    case _: Ident    => "IDENT"
    case _: IntTok   => "INT"
    case _: ErrorTok => "ERROR"
    case _: Eof      => "EOF"
  }

  override def toString: String =
    s"""{"kind": "$kind", "value": "$lexeme", "line": $line, "column": $column}"""
}
