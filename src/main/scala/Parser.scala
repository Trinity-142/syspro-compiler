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
def parseProgram(state: ParserState, program: List[Stmt]): (List[Stmt], List[String]) = state.tokens match {
    case (_: Token.Eof) :: _ | Nil =>
      (program.reverse, state.errors.reverse)

    case _ =>
      val (stmt, nextState) = parseStmt(state)
      parseProgram(nextState, stmt :: program)
}

def parseStmt(state: ParserState): (Stmt, ParserState) = state.tokens match {
    case (kw: Token.Return) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Return(kw, expr), expectSemi(nextState))

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: (_: Token.Assign) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Decl(kw, ident, expr), expectSemi(nextState))

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: tail =>
      val msg = s"Parser error at ${ident.line}:${ident.column}: '=' expected after declaration"
      (Stmt.Decl(kw, ident, Expr.ErrorExpr(ident)), expectSemi(state.next(tail).withError(msg)))

    case (ident: Token.Ident) :: (assign: Token.Assign) :: tail =>
      val (expr, nextState) = parseExpr(0, state.next(tail))
      (Stmt.Assign(ident, assign, expr), expectSemi(nextState))

    case (kw: Token.If) :: tail => parseIf(kw, state.next(tail))

    case (kw: Token.While) :: tail => parseWhile(kw, state.next(tail))

    case (kw: Token.Break) :: tail => (Stmt.Break(kw), expectSemi(state.next(tail)))

    case (kw: Token.Continue) :: tail => (Stmt.Continue(kw), expectSemi(state.next(tail)))

    case (lbrace: Token.Lbrace) :: tail => parseBlock(lbrace, state.next(tail), Nil)

    case (_: Token.IntTok | _: Token.Ident | _: Token.Minus | _: Token.Lparen |
          _: Token.True | _: Token.False | _: Token.Not) :: _ =>
      val (expr, nextState) = parseExpr(0, state)
      (Stmt.ExprStmt(expr), expectSemi(nextState))

    case (badToken: Token) :: tail =>
      val msg = badToken match {
        case err: Token.ErrorTok => s"Error at ${err.line}:${err.column}: ${err.msg}"
        case _ => s"Parser error at ${badToken.line}:${badToken.column}: unexpected token '${badToken.lexeme}'"
      }
      (Stmt.ErrorStmt(badToken), state.withErrorAndSync(msg, tail))

    case Nil =>
      val msg = "Compiler error: statement expected, but token stream exhausted"
      (Stmt.ErrorStmt(Token.Eof(0, 0)), state.withError(msg))
}

def parseIf(kw: Token.If, state: ParserState): (Stmt, ParserState) = {
  val (cond, stateAfterCond) = parseParenExpr(state)
  val (thenBranch, stateAfterThen) = parseStmt(stateAfterCond)

  stateAfterThen.tokens match {
    case (_: Token.Else) :: elseTail =>
      val (elseBranch, stateAfterElse) = parseStmt(stateAfterThen.next(elseTail))
      (Stmt.If(kw, cond, thenBranch, Some(elseBranch)), stateAfterElse)
    case _ =>
      (Stmt.If(kw, cond, thenBranch, None), stateAfterThen)
  }
}

def parseWhile(kw: Token.While, state: ParserState): (Stmt, ParserState) = {
  val (cond, stateAfterCond) = parseParenExpr(state)
  val (body, stateAfterBody) = parseStmt(stateAfterCond)
  (Stmt.While(kw, cond, body), stateAfterBody)
}

@tailrec
def parseBlock(lbrace: Token.Lbrace, state: ParserState, acc: List[Stmt]): (Stmt, ParserState) = state.tokens match {
  case (_: Token.Rbrace) :: tail =>
    (Stmt.Block(lbrace, acc.reverse), state.next(tail))

  case (eof: Token.Eof) :: _ =>
    val msg = s"Error at ${eof.line}:${eof.column}: '}' expected, but 'EOF' found"
    (Stmt.Block(lbrace, acc.reverse), state.withError(msg))

  case Nil =>
    val msg = "Compiler error: '}' expected, but token stream exhausted"
    (Stmt.Block(lbrace, acc.reverse), state.withError(msg))

  case _ =>
    val (stmt, nextState) = parseStmt(state)
    parseBlock(lbrace, nextState, stmt :: acc)
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
  case (_: Token.Rbrace | _: Token.Eof) :: tail => tokens
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
        val (rhs, nextState) = parseExpr(prec + 1, currState.next(tail))
        precedenceClimbing(Expr.Binary(lhs, head, rhs), nextState)
      } else (lhs, currState)

    case Nil => (lhs, currState)
  }

  precedenceClimbing(lhs, rest)
}

def parseUnary(state: ParserState): (Expr, ParserState) = state.tokens match {
  case (intLiteral: Token.IntTok) :: tail =>
    (Expr.IntLiteral(intLiteral.value.toLong, intLiteral), state.next(tail))

  case (boolTrue: Token.True) :: tail =>
    (Expr.BoolLiteral(true, boolTrue), state.next(tail))

  case (boolFalse: Token.False) :: tail =>
    (Expr.BoolLiteral(false, boolFalse), state.next(tail))

  case (ident: Token.Ident) :: tail =>
    (Expr.Ident(ident), state.next(tail))

  case (unaryOp: (Token.Minus | Token.Not)) :: tail =>
    val (expr, nextState) = parseUnary(state.next(tail))
    (Expr.Unary(unaryOp, expr), nextState)

  case (_: Token.Lparen) :: _ =>
    parseParenExpr(state)

  case unexpected :: _ =>
    val msg = s"Error at ${unexpected.line}:${unexpected.column}: expression expected, found '${unexpected.lexeme}'"
    (Expr.ErrorExpr(unexpected), state.withError(msg))

  case Nil =>
    val msg = "Compiler error: token stream unexpectedly exhausted"
    (Expr.ErrorExpr(Token.Eof(0, 0)), state.withError(msg))
}

def parseParenExpr(state: ParserState): (Expr, ParserState) = state.tokens match {
  case (_: Token.Lparen) :: tail =>
    val (expr, nextState) = parseExpr(0, state.next(tail))
    expectRparen(expr, nextState)

  case unexpected :: _ =>
    val msg = s"Error at ${unexpected.line}:${unexpected.column}: '(' expected, but '${unexpected.lexeme}' found"
    val (expr, nextState) = parseExpr(0, state)
    (Expr.ErrorExpr(unexpected), nextState.withError(msg))

  case Nil =>
    val msg = "Compiler error: '(' expected, but token stream exhausted"
    (Expr.ErrorExpr(Token.Eof(0, 0)), state.withError(msg))
}

def expectRparen(expr: Expr, state: ParserState): (Expr, ParserState) = state.tokens match {
  case (_: Token.Rparen) :: tail =>
    (expr, state.next(tail))
    
  case unexpected :: _ =>
    val msg = s"Error at ${unexpected.line}:${unexpected.column}: ')' expected, but '${unexpected.lexeme}' found"
    (Expr.ErrorExpr(unexpected), state.withError(msg))

  case Nil =>
    val msg = "Compiler error: ')' expected, but token stream exhausted"
    (Expr.ErrorExpr(Token.Eof(0, 0)), state.withError(msg))
}

def getPrec(token: Token): Int = token match {
  case _: Token.Mult | _: Token.Div                          => 60
  case _: Token.Plus | _: Token.Minus                        => 55
  case _: Token.Lt | _: Token.Gt | _: Token.Le | _: Token.Ge => 50
  case _: Token.Eq | _: Token.Ne                             => 45
  case _: Token.And                                          => 40
  case _: Token.Or                                           => 35
  case _                                                     => 0
}
