enum Token(val line: Int, val column: Int) {
  case Rparen(l: Int, c: Int) extends Token(l, c)
  case Lparen(l: Int, c: Int) extends Token(l, c)
  case Assign(l: Int, c: Int) extends Token(l, c)
  case Semi(l: Int, c: Int) extends Token(l, c)
  case Plus(l: Int, c: Int) extends Token(l, c)
  case Minus(l: Int, c: Int) extends Token(l, c)
  case Mult(l: Int, c: Int) extends Token(l, c)
  case Div(l: Int, c: Int) extends Token(l, c)
  case Val(l: Int, c: Int) extends Token(l, c)
  case Var(l: Int, c: Int) extends Token(l, c)
  case Return(l: Int, c: Int) extends Token(l, c)

  case Ident(value: String, l: Int, c: Int) extends Token(l, c)
  case IntTok(value: String, l: Int, c: Int) extends Token(l, c)
  case ErrorTok(msg: String, l: Int, c: Int) extends Token(l, c)
  case Eof(l: Int, c: Int) extends Token(l, c)

  def lexeme: String = this match {
    case _: Rparen => ")"
    case _: Lparen => "("
    case _: Assign => "="
    case _: Semi => ";"
    case _: Plus => "+"
    case _: Minus => "-"
    case _: Mult => "*"
    case _: Div => "/"
    case _: Val => "val"
    case _: Var => "var"
    case _: Return => "return"
    case Ident(v, _, _) => v
    case IntTok(v, _, _) => v
    case ErrorTok(msg, _, _) => msg
    case _: Eof => "EOF"
  }

  def kind: String = this match {
    case _: Rparen => "RPAREN"
    case _: Lparen => "LPAREN"
    case _: Assign => "ASSIGN"
    case _: Semi => "SEMI"
    case _: Plus => "PLUS"
    case _: Minus => "MINUS"
    case _: Mult => "MULT"
    case _: Div => "DIV"
    case _: Val => "VAL"
    case _: Var => "VAR"
    case _: Return => "RETURN"
    case _: Ident => "IDENT"
    case _: IntTok => "INT"
    case _: ErrorTok => "ERROR"
    case _: Eof => "EOF"
  }

  override def toString: String =
    s"""{"kind": "$kind", "value": "$lexeme", "line": $line, "column": $column}"""
}
