import scala.annotation.tailrec

case class ParserState(tokens: List[Token], errors: List[String]) {
  def next(newTokens: List[Token]): ParserState =
    this.copy(tokens = newTokens)

  def withError(msg: String): ParserState =
    this.copy(errors = msg :: this.errors)
   
  def withErrorAndSync(msg: String, tailToSync: List[Token]): ParserState =
    this.copy(
      tokens = synchronize(tailToSync),
      errors = msg :: this.errors
    )
}



// ------------ STATEMENTS ------------

@tailrec
def parseProgram(state: ParserState, program: List[Stmt]): (List[Stmt], ParserState) = state.tokens match {
    case (_: Token.Eof) :: _ =>
      (program.reverse, state)

    case Nil =>
      (program.reverse, state)

    case _ =>
      val (stmt, stateAfterStmt) = parseStmt(state)
      stmt match {
        case err: Stmt.ErrorStmt =>
          parseProgram(stateAfterStmt, err :: program)
        case validStmt =>
          val nextState = expectSemi(stateAfterStmt)
          parseProgram(nextState, validStmt :: program)
      }
}

def parseStmt(state: ParserState): (Stmt, ParserState) = state.tokens match {
    case (kw: Token.Return) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Return(kw, expr), nextState)

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: (_: Token.Assign) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Decl(kw, ident, expr), nextState)

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: tail =>
      val msg = s"Parser error at ${ident.line}:${ident.column}: '=' expected after declaration"
      (Stmt.Decl(kw, ident, Expr.ErrorExpr(ident)), state.withErrorAndSync(msg, tail))

    case (ident: Token.Ident) :: (_: Token.Assign) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Assign(ident, expr), nextState)

    case (_: Token.IntTok | _: Token.Ident | _: Token.Minus | _: Token.Lparen) :: _ =>
      val (expr, nextState) = parseExpr(0, state)
      (Stmt.ExprStmt(expr), nextState)

    case (badToken: Token) :: tail =>
      val msg = badToken match {
        case err: Token.ErrorTok => s"Error at $err.line:$err.column: $err.msg"
        case _ => s"Parser error at ${badToken.line}:${badToken.column}: unexpected token '${badToken.lexeme}'"
      }
      (Stmt.ErrorStmt(badToken), state.withErrorAndSync(msg, tail))

    case Nil =>
      val msg = "Compiler error: statement expected, but token stream exhausted"
      (Stmt.ErrorStmt(Token.Eof(0, 0)), state.withError(msg))
}

def expectSemi(state: ParserState): ParserState = state.tokens match {
    case (_: Token.Semi) :: tail =>
      state.next(tail)

    case unexpected :: _ =>
      val msg = s"Error at ${unexpected.line}:${unexpected.column}: ';' expected, but '${unexpected.lexeme}' found"
      state.withErrorAndSync(msg, state.tokens)

    case Nil =>
      val msg = "Compiler error: ';' expected, but token stream exhausted"
      state.withError(msg)
}

@tailrec
def synchronize(tokens: List[Token]): List[Token] = tokens match {
  case (_: Token.Semi) :: tail => tail
  case (_: Token.Eof) :: tail => tokens
  case _ :: tail => synchronize(tail)
  case Nil => Nil
}



// ------------ EXPRESSIONS ------------

def parseExpr(minPrec: Int, state: ParserState): (Expr, ParserState) = {
  val (lhs, rest) = parseUnary(state)

  @tailrec
  def precedenceClimbing(lhs: Expr, currState: ParserState): (Expr, ParserState) = currState.tokens match {
    case head :: tail =>
      val prec = getPrec(head)
      if (prec > 0 && prec >= minPrec) {
        val (rhs, nextState) = parseExpr(prec + 1, state.next(tail))
        precedenceClimbing(Expr.Binary(lhs, head, rhs), nextState)
      } else (lhs, currState)

    case Nil => (lhs, currState)
  }

  precedenceClimbing(lhs, rest)
}

def parseUnary(state: ParserState): (Expr, ParserState) = state.tokens match {
  case (intLiteral: Token.IntTok) :: tail =>
    (Expr.IntLiteral(intLiteral.value.toLong, intLiteral), state.next(tail))

  case (ident: Token.Ident) :: tail =>
    (Expr.Ident(ident), state.next(tail))

  case (unaryMinus: Token.Minus) :: tail =>
    val (expr, nextState) = parseUnary(state.next(tail))
    (Expr.Unary(unaryMinus, expr), nextState)

  case (lparen: Token.Lparen) :: tail =>
    val (expr, nextState) = parseExpr(0, state.next(tail))
    expectRparen(expr, nextState)

  case unexpected :: _ =>
    val msg = s"Error at ${unexpected.line}:${unexpected.column}: expression expected, found '${unexpected.lexeme}'"
    (Expr.ErrorExpr(unexpected), state.withError(msg))

  case Nil =>
    val msg = "Compiler error: token stream unexpectedly exhausted"
    (Expr.ErrorExpr(Token.Eof(0, 0)), state.withError(msg))
}

def expectRparen(expr: Expr, state: ParserState): (Expr, ParserState) = state.tokens match {
  case (_: Token.Rparen) :: tail =>
    (expr, state.next(tail))
    
  case unexpected :: _ =>
    val msg = s"Error at ${unexpected.line}:${unexpected.column}: ')' expected, but '${unexpected.lexeme}' found"
    (expr, state.withError(msg))
    
  case Nil =>
    val msg = "Compiler error: token stream unexpectedly exhausted"
    (expr, state.withError(msg))
}

def getPrec(token: Token): Int = token match {
  case _: Token.Mult | _: Token.Div   => 20
  case _: Token.Plus | _: Token.Minus => 10
  case _                              => 0
}
