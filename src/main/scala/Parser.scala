import scala.annotation.tailrec
import cats.syntax.all.*
import cats.data.{EitherT, State}

import scala.reflect.ClassTag

case class ParserState(tokens: List[Token], errors: List[String] = Nil) {
  def next(newTokens: List[Token]): ParserState =
    this.copy(tokens = newTokens)

  def withError(msg: String): ParserState =
    this.copy(errors = msg :: this.errors)
}

type Parser[A] = State[ParserState, A]

type ParseOrError[A] = EitherT[Parser, Token, A]

enum SyncMode {
  case TopLevel, Stmt, None
}

extension [A](ps: ParseOrError[A]) {
  def recoverWithSync[B >: A](onError: Token => B, mode: SyncMode): ParseOrError[B] =
    EitherT.liftF(
      ps.value.flatMap {
        case Right(value) => State.pure(value: B)
        case Left(errTok) =>
          State.modify[ParserState] { s =>
            mode match {
              case SyncMode.TopLevel => s.next(syncTopLevel(s.tokens))
              case SyncMode.Stmt     => s.next(syncStmt(s.tokens))
              case SyncMode.None     => s
            }
          }.as(onError(errTok))
      }
    )
}

extension [A <: TopDecl](ps: ParseOrError[A]) {
  def recoverToDecl: ParseOrError[TopDecl] =
    ps.recoverWithSync(TopDecl.ErrorDecl(_), SyncMode.TopLevel)
}

extension [A <: Stmt](ps: ParseOrError[A]) {
  def recoverToStmt: ParseOrError[Stmt] =
    ps.recoverWithSync(Stmt.ErrorStmt(_), SyncMode.Stmt)
}

extension [A <: Expr](ps: ParseOrError[A]) {
  def recoverToExpr: ParseOrError[Expr] =
    ps.recoverWithSync(Expr.ErrorExpr(_), SyncMode.None)
}

val peek: ParseOrError[Option[Token]] =
  EitherT.liftF(State.inspect(_.tokens.headOption))

def pure[A](value: A): ParseOrError[A] =
  EitherT.pure[Parser, Token](value)

def modifyState(f: ParserState => ParserState): ParseOrError[Unit] =
  EitherT.liftF(State.modify(f))

def expect[A](expectation: String)(pf: PartialFunction[Token, A]): ParseOrError[A] = EitherT(State { state =>
  state.tokens match {
    case head :: tail if pf.isDefinedAt(head) =>
      (state.next(tail), Right(pf(head)))
    case (err: Token.ErrorTok) :: tail =>
      val msg = s"Error at ${err.line}:${err.column}: ${err.msg}"
      (state.next(tail).withError(msg), Left(err))
    case unexpected :: _ =>
      val msg = s"Parser error at ${unexpected.line}:${unexpected.column}: $expectation expected, but '${unexpected.lexeme}' found"
      (state.withError(msg), Left(unexpected))
    case Nil =>
      val eof = Token.Eof(0, 0)
      val msg = s"Compiler error: $expectation expected, but token stream exhausted"
      (state.withError(msg), Left(eof))
  }
})

def expect[T <: Token : ClassTag](expectation: String): ParseOrError[T] =
  expect(expectation) { case t: T => t }

def expectOpt[T <: Token : ClassTag]: ParseOrError[Option[T]] = EitherT.liftF(State { state =>
    state.tokens match {
      case (head: T) :: tail => (state.next(tail), Some(head))
      case _                 => (state, None)
    }
})

def commaSeparatedUntilRparen[A](parseItem: ParseOrError[A]): ParseOrError[List[A]] = peek.flatMap {
  case Some(_: Token.Rparen) => pure(Nil)
  case _ =>
    def loop(acc: List[A]): ParseOrError[List[A]] = for {
      item     <- parseItem
      commaOpt <- expectOpt[Token.Comma]
      res      <- commaOpt match {
        case Some(_) => loop(item :: acc)
        case None    => pure((item :: acc).reverse)
      }
    } yield res
    loop(Nil)
}

@tailrec
def syncStmt(tokens: List[Token]): List[Token] = tokens match {
  case (_: Token.Semi) :: tail => tail
  case (_: Token.Rbrace | _: Token.Def | _: Token.Extern | _: Token.Eof) :: _ => tokens
  case _ :: tail => syncStmt(tail)
  case Nil       => Nil
}

@tailrec
def syncTopLevel(tokens: List[Token]): List[Token] = tokens match {
  case (_: Token.Def | _: Token.Extern | _: Token.Eof) :: _ => tokens
  case _ :: tail => syncTopLevel(tail)
  case Nil       => Nil
}


// ------------ TOP-LEVEL DECLARATIONS ------------

def parseProgram(initialState: ParserState): (List[TopDecl], List[String]) = {
  def loop(acc: List[TopDecl] = Nil): ParseOrError[List[TopDecl]] = peek.flatMap {
    case Some(_: Token.Eof) | None => pure(acc.reverse)
    case _                         => parseDecl.flatMap(d => loop(d :: acc))
  }

  val (finalState, declsEither) = loop().value.run(initialState).value
  val decls = declsEither.getOrElse(Nil)
  (decls, finalState.errors.reverse)
}

def parseParams: ParseOrError[List[Expr.Param]] =
  commaSeparatedUntilRparen(expect[Token.Ident]("parameter name").map(Expr.Param(_)))

def parseDecl: ParseOrError[TopDecl] = peek.flatMap {
  case Some(_: Token.Def) =>
    (for {
      defKw  <- expect[Token.Def]("'def'")
      ident  <- expect[Token.Ident]("function name")
      _      <- expect[Token.Lparen]("'('")
      params <- parseParams
      _      <- expect[Token.Rparen]("')'")
      body   <- parseBlock
    } yield TopDecl.FuncDecl(defKw, ident, params, body)).recoverToDecl

  case Some(_: Token.Extern) =>
    (for {
      extKw  <- expect[Token.Extern]("'extern'")
      _      <- expect[Token.Def]("'def'")
      ident  <- expect[Token.Ident]("function name")
      _      <- expect[Token.Lparen]("'('")
      params <- parseParams
      _      <- expect[Token.Rparen]("')'")
      _      <- expect[Token.Semi]("';'")
    } yield TopDecl.ExternDecl(extKw, ident, params)).recoverToDecl

  case _ =>
    expect[TopDecl]("'def' or 'extern'")(PartialFunction.empty).recoverToDecl
}

// ------------ STATEMENTS ------------

def parseBlock: ParseOrError[Stmt.Block] = {
  def loop(acc: List[Stmt] = Nil): ParseOrError[List[Stmt]] = expectOpt[Token.Rbrace].flatMap {
    case Some(_) => pure(acc.reverse)
    case None => peek.flatMap {
      case Some(unclosed: (Token.Eof | Token.Def | Token.Extern)) =>
        val msg = s"Error at ${unclosed.line}:${unclosed.column}: '}' expected, but '${unclosed.lexeme}' found"
        modifyState(_.withError(msg)).as(acc.reverse)
      case None =>
        val msg = "Compiler error: '}' expected, but token stream exhausted"
        modifyState(_.withError(msg)).as(acc.reverse)
      case _ =>
        parseStmt.flatMap(stmt => loop(stmt :: acc))
    }
  }

  for {
    lbrace <- expect[Token.Lbrace]("'{'")
    stmts  <- loop()
  } yield Stmt.Block(lbrace, stmts)
}

def parseStmt: ParseOrError[Stmt] = peek.flatMap {
  case Some(_: Token.Return) =>
    (for {
      kw   <- expect[Token.Return]("'return'")
      expr <- parseExpr(0)
      _    <- expect[Token.Semi]("';'")
    } yield Stmt.Return(kw, expr)).recoverToStmt

  case Some(_: (Token.Val | Token.Var)) =>
    (for {
      kw    <- expect("declaration keyword") { case k: (Token.Val | Token.Var) => k }
      ident <- expect[Token.Ident]("variable name")
      _     <- expect[Token.Assign]("'=' after declaration")
      expr  <- parseExpr(0)
      _     <- expect[Token.Semi]("';'")
    } yield Stmt.Decl(kw, ident, expr)).recoverToStmt

  case Some(_: Token.If) =>
    (for {
      kw         <- expect[Token.If]("'if'")
      cond       <- parseParenExpr
      thenBranch <- parseStmt
      elseOpt    <- expectOpt[Token.Else]
      elseBranch <- elseOpt.traverse(_ => parseStmt)
    } yield Stmt.If(kw, cond, thenBranch, elseBranch)).recoverToStmt

  case Some(_: Token.While) =>
    (for {
      kw   <- expect[Token.While]("'while'")
      cond <- parseParenExpr
      body <- parseStmt
    } yield Stmt.While(kw, cond, body)).recoverToStmt

  case Some(_: Token.Break) =>
    (for {
      kw <- expect[Token.Break]("'break'")
      _  <- expect[Token.Semi]("';'")
    } yield Stmt.Break(kw)).recoverToStmt

  case Some(_: Token.Continue) =>
    (for {
      kw <- expect[Token.Continue]("'continue'")
      _  <- expect[Token.Semi]("';'")
    } yield Stmt.Continue(kw)).recoverToStmt

  case Some(_: Token.Lbrace) =>
    parseBlock.recoverToStmt

  // Case for Assign or ExprStmt
  case Some(_: (Token.IntTok | Token.Ident | Token.Minus | Token.Lparen |
                  Token.True | Token.False | Token.Not)) => (for {
    lhs <- parseExpr(0)
    stmt <- lhs match {
      case Expr.Ident(ident) => expectOpt[Token.Assign].flatMap {
        case Some(assign) => for {
          rhs <- parseExpr(0)
          _   <- expect[Token.Semi]("';'")
        } yield Stmt.Assign(ident, assign, rhs)
        case None =>
          expect[Token.Semi]("';'").as(Stmt.ExprStmt(lhs))
      }
      case _ =>
        expect[Token.Semi]("';'").as(Stmt.ExprStmt(lhs))
    }
  } yield stmt).recoverToStmt

  case _ =>
    expect[Stmt]("statement")(PartialFunction.empty).recoverToStmt
}

// ------------ EXPRESSIONS ------------

def parseExpr(minPrec: Int): ParseOrError[Expr] = {
  def climb(lhs: Expr): ParseOrError[Expr] = peek.flatMap {
    case Some(op) if getPrec(op) > 0 && getPrec(op) >= minPrec =>
      for {
        _    <- modifyState(s => s.next(s.tokens.tail))
        rhs  <- parseExpr(getPrec(op) + 1)
        next <- climb(Expr.Binary(lhs, op, rhs))
      } yield next
    case _ =>
      pure(lhs)
  }

  parseUnary.flatMap(climb)
}

def parseUnary: ParseOrError[Expr] = peek.flatMap {
  case Some(_: Token.IntTok) =>
    expect[Token.IntTok]("integer literal").map(int => Expr.IntLiteral(int.value.toLong, int)).recoverToExpr

  case Some(_: Token.True) =>
    expect[Token.True]("'true'").map(Expr.BoolLiteral(true, _)).recoverToExpr

  case Some(_: Token.False) =>
    expect[Token.False]("'false'").map(Expr.BoolLiteral(false, _)).recoverToExpr

  case Some(_: Token.Ident) =>
    (for {
      ident  <- expect[Token.Ident]("identifier")
      lparen <- expectOpt[Token.Lparen]
      expr   <- lparen match {
        case Some(_) =>
          for {
            args <- parseArgs
            _    <- expect[Token.Rparen]("')'")
          } yield Expr.Call(ident, args)
        case None =>
          pure(Expr.Ident(ident))
      }
    } yield expr).recoverToExpr

  case Some(_: (Token.Minus | Token.Not)) =>
    (for {
      op   <- expect("unary operator") { case u: (Token.Minus | Token.Not) => u }
      expr <- parseUnary
    } yield Expr.Unary(op, expr)).recoverToExpr

  case Some(_: Token.Lparen) =>
    parseParenExpr

  case _ =>
    expect[Expr]("expression")(PartialFunction.empty).recoverToExpr
}

val parseArgs: ParseOrError[List[Expr]] =
  commaSeparatedUntilRparen(parseExpr(0))

def parseParenExpr: ParseOrError[Expr] =
  (for {
    _    <- expect[Token.Lparen]("'('")
    expr <- parseExpr(0)
    _    <- expect[Token.Rparen]("')'")
  } yield expr).recoverToExpr

def getPrec(token: Token): Int = token match {
  case _: Token.Mult | _: Token.Div                          => 60
  case _: Token.Plus | _: Token.Minus                        => 55
  case _: Token.Lt | _: Token.Gt | _: Token.Le | _: Token.Ge => 50
  case _: Token.Eq | _: Token.Ne                             => 45
  case _: Token.And                                          => 40
  case _: Token.Or                                           => 35
  case _                                                     => 0
}
