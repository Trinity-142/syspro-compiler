import scala.annotation.tailrec

//           >_<
var hasParserErrors: Boolean = false



// ------------ STATEMENTS ------------

@tailrec
def parseProgram(tokens: List[Token], program: List[Stmt]): List[Stmt] = tokens match {
    case (_: Token.Eof) :: _ =>
      program.reverse

    case Nil =>
      program.reverse

    case _ =>
      val (stmt, rest) = parseStmt(tokens)
      stmt match {
        case err @ Stmt.ErrorStmt() =>
          parseProgram(rest, err :: program)
        case validStmt =>
          val (stmtsToAdd, nextTokens) = expectSemi(validStmt, rest)
          parseProgram(nextTokens, stmtsToAdd.reverse ::: program)
      }
}

def parseStmt(tokens: List[Token]): (Stmt, List[Token]) = tokens match {
    case (kw: Token.Return) :: tail =>
      val (expr, rest) = parseExpr(0, tail)
      (Stmt.Return(kw, expr), rest)

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: (_: Token.Assign) :: tail =>
      val (expr, rest) = parseExpr(0, tail)
      (Stmt.Decl(kw, ident, expr), rest)

    case (kw: (Token.Val | Token.Var)) :: (ident: Token.Ident) :: tail =>
      System.err.println(s"Parser error at ${ident.line}:${ident.column}: '=' expected after declaration")
      hasParserErrors = true
      (Stmt.Decl(kw, ident, Expr.ErrorExpr()), synchronize(tail))

    case (ident: Token.Ident) :: (_: Token.Assign) :: tail =>
      val (expr, rest) = parseExpr(0, tail)
      (Stmt.Assign(ident, expr), rest)

    case (_: Token.IntTok | _: Token.Ident | _: Token.Minus | _: Token.Lparen) :: _ =>
      val (expr, rest) = parseExpr(0, tokens)
      (Stmt.ExprStmt(expr), rest)

    case (err: Token.ErrorTok) :: tail =>
      System.err.println(s"Error at $err.line:$err.column: $err.msg")
      hasParserErrors = true
      (Stmt.ErrorStmt(), synchronize(tail))

    case unexpected :: tail =>
      System.err.println(s"Parser error at ${unexpected.line}:${unexpected.column}: unexpected token '${unexpected.lexeme}'")
      hasParserErrors = true
      (Stmt.ErrorStmt(), synchronize(tail))

    case Nil =>
      System.err.println("Compiler error: statement expected, but token stream exhausted")
      hasParserErrors = true
      (Stmt.ErrorStmt(), Nil)
}

def expectSemi(validStmt: Stmt, tokens: List[Token]): (List[Stmt], List[Token]) = tokens match {
    case (_: Token.Semi) :: tail =>
      (List(validStmt), tail)

    case unexpected :: _ =>
      System.err.println(s"Error at ${unexpected.line}:${unexpected.column}: ';' expected, but '${unexpected.lexeme}' found")
      hasParserErrors = true
      (List(validStmt), synchronize(tokens))

    case Nil =>
      System.err.println("Compiler error: ';' expected, but token stream exhausted")
      hasParserErrors = true
      (List(validStmt), Nil)
}

@tailrec
def synchronize(tokens: List[Token]): List[Token] = tokens match {
  case (_: Token.Semi) :: tail => tail
  case (_: Token.Eof) :: tail => tokens
  case _ :: tail => synchronize(tail)
  case Nil => Nil
}



// ------------ EXPRESSIONS ------------

def parseExpr(minPrec: Int, tokens: List[Token]): (Expr, List[Token]) = {
  val (lhs, rest) = parseUnary(tokens)

  @tailrec
  def precedenceClimbing(lhs: Expr, currRest: List[Token]): (Expr, List[Token]) = currRest match {
    case head :: tail =>
      val prec = getPrec(head)
      if (prec > 0 && prec >= minPrec) {
        val (rhs, nextRest) = parseExpr(prec + 1, tail)
        precedenceClimbing(Expr.Binary(lhs, head, rhs), nextRest)
      } else (lhs, currRest)

    case Nil => (lhs, currRest)
  }

  precedenceClimbing(lhs, rest)
}

def parseUnary(tokens: List[Token]): (Expr, List[Token]) = tokens match {
  case (intLiteral: Token.IntTok) :: tail =>
    (Expr.IntLiteral(intLiteral.value.toLong, intLiteral), tail)

  case (ident: Token.Ident) :: tail =>
    (Expr.Ident(ident), tail)

  case (unaryMinus: Token.Minus) :: tail =>
    val (expr, rest) = parseUnary(tail)
    (Expr.Unary(unaryMinus, expr), rest)

  case (lparen: Token.Lparen) :: tail =>
    val (expr, rest) = parseExpr(0, tail)
    expectRparen(expr, rest)

  case unexpected :: _ =>
    System.err.println(s"Error at ${unexpected.line}:${unexpected.column}: expression expected, found '${unexpected.lexeme}'")
    hasParserErrors = true
    (Expr.ErrorExpr(), tokens)

  case Nil =>
    System.err.println("Compiler error: token stream unexpectedly exhausted")
    hasParserErrors = true
    (Expr.ErrorExpr(), Nil)
}

def expectRparen(expr: Expr, tokens: List[Token]): (Expr, List[Token]) = tokens match {
  case (_: Token.Rparen) :: tail =>
    (expr, tail)
    
  case unexpected :: _ =>
    System.err.println(s"Error at ${unexpected.line}:${unexpected.column}: ')' expected, but '${unexpected.lexeme}' found")
    hasParserErrors = true
    (expr, tokens)
    
  case Nil =>
    System.err.println("Compiler error: token stream unexpectedly exhausted")
    hasParserErrors = true
    (expr, Nil)
}

def getPrec(token: Token): Int = token match {
  case _: Token.Mult | _: Token.Div   => 20
  case _: Token.Plus | _: Token.Minus => 10
  case _                              => 0
}
