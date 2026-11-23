import cats.data.State
import cats.syntax.all.*

object AstAnalyzer {
  private case class AnalyzerState(
    symbolTable: Map[String, Boolean] = Map.empty,  // variable name -> mutability
    functions: Map[String, Int] = Map.empty,        // func name -> arity
    errors: List[String] = Nil,
    loopDepth: Int = 0
  )

  private type Analyzer[A] = State[AnalyzerState, A]

  private val noop: Analyzer[Unit] = State.pure(())

  private def inspect[A](f: AnalyzerState => A): Analyzer[A] = State.inspect(f)

  private def modify(f: AnalyzerState => AnalyzerState): Analyzer[Unit] = State.modify(f)

  // ------------ UTILITIES ------------

  private def addError(msg: String): Analyzer[Unit] =
    modify(s => s.copy(errors = msg :: s.errors))

  private def withScope[A](action: Analyzer[A]): Analyzer[A] = for {
    savedSymbols <- inspect(_.symbolTable)
    res          <- action
    _            <- modify(_.copy(symbolTable = savedSymbols))
  } yield res

  private def withLoop[A](action: Analyzer[A]): Analyzer[A] = for {
    _   <- modify(s => s.copy(loopDepth = s.loopDepth + 1))
    res <- action
    _   <- modify(s => s.copy(loopDepth = s.loopDepth - 1))
  } yield res

  private def registerFunc(ident: Token.Ident, arity: Int): Analyzer[Unit] = for {
    exists <- inspect(_.functions.contains(ident.lexeme))
    _ <- if (exists)
      addError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' is already defined")
    else
      modify(s => s.copy(functions = s.functions + (ident.lexeme -> arity)))
  } yield ()

  private def registerParams(params: List[Expr.Param]): Analyzer[Unit] =
    params.traverse_ { case Expr.Param(pIdent) =>
      for {
        exists <- inspect(_.symbolTable.contains(pIdent.lexeme))
        _ <- if (exists)
          addError(s"Semantic error at ${pIdent.line}:${pIdent.column}: parameter '${pIdent.lexeme}' is already defined")
        else
          modify(s => s.copy(symbolTable = s.symbolTable + (pIdent.lexeme -> true)))
      } yield ()
    }

  // ------------ ENTRY POINTS ------------
  
  def analyzeProgram(decls: List[TopDecl]): List[String] = {
    val hasMain = decls.exists {
      case TopDecl.FuncDecl(_, ident, _, _) => ident.lexeme == "main"
      case _                                => false
    }

    val programAnalysis: Analyzer[Unit] = for {
      _ <- decls.traverse_ {
        case TopDecl.FuncDecl(_, ident, params, _) => registerFunc(ident, params.length)
        case TopDecl.ExternDecl(_, ident, params)  => registerFunc(ident, params.length)
        case TopDecl.ErrorDecl(_)                  => noop
      }
      _ <- if (hasMain) noop else addError("Semantic error: 'main' function must be explicitly defined")
      _ <- decls.traverse_(analyzeDecl)
    } yield ()

    programAnalysis.runS(AnalyzerState()).value.errors.reverse
  }

  def analyzeStmts(stmts: List[Stmt]): List[String] =
    stmts.traverse_(analyze).runS(AnalyzerState()).value.errors.reverse

  // ------------ TOP-LEVEL DECLARATIONS ------------
  
  private def analyzeDecl(decl: TopDecl): Analyzer[Unit] = decl match {
    case TopDecl.ExternDecl(_, _, params) =>
      withScope(registerParams(params))

    case TopDecl.FuncDecl(_, ident, params, body) =>
      withScope {
        for {
          _ <- registerParams(params)
          _ <- analyze(body)
          _ <- if (hasReturnOnAllPaths(body)) noop
          else addError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' must end with a 'return' statement on all paths")
        } yield ()
      }

    case TopDecl.ErrorDecl(_) => noop
  }

  private def hasReturnOnAllPaths(stmt: Stmt): Boolean = stmt match {
    case _: Stmt.Return                              => true
    case Stmt.Block(_, stmts)                        => stmts.exists(hasReturnOnAllPaths)
    case Stmt.If(_, _, thenBranch, Some(elseBranch)) => hasReturnOnAllPaths(thenBranch) && hasReturnOnAllPaths(elseBranch)
    case _                                           => false
  }

  private def analyze(node: Stmt | Expr): Analyzer[Unit] = node match {

    // ------------ STATEMENTS ------------
    
    case Stmt.Decl(keyWord, ident, value) => for {
      _      <- analyze(value)
      exists <- inspect(_.symbolTable.contains(ident.lexeme))
      _ <- if (!exists) {
        val isMutable = keyWord.isInstanceOf[Token.Var]
        modify(s => s.copy(symbolTable = s.symbolTable + (ident.lexeme -> isMutable)))
      } else {
        addError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' is already defined")
      }
    } yield ()

    case Stmt.Assign(ident, _, value) => for {
      mutability <- inspect(_.symbolTable.get(ident.lexeme))
      _ <- mutability match {
        case None =>
          addError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
        case Some(false) =>
          addError(s"Semantic error at ${ident.line}:${ident.column}: reassignment to val '${ident.lexeme}'")
        case Some(true) =>
          noop
      }
      _ <- analyze(value)
    } yield ()

    case Stmt.If(_, cond, thenBranch, elseBranch) => for {
      _ <- analyze(cond)
      _ <- withScope(analyze(thenBranch))
      _ <- elseBranch.traverse_(s => withScope(analyze(s)))
    } yield ()

    case Stmt.While(_, cond, body) => for {
      _ <- analyze(cond)
      _ <- withLoop(withScope(analyze(body)))
    } yield ()

    case Stmt.Break(kw) =>
      inspect(_.loopDepth).flatMap { depth =>
        if (depth == 0) addError(s"Semantic error at ${kw.line}:${kw.column}: 'break' outside of loop")
        else noop
      }

    case Stmt.Continue(kw) =>
      inspect(_.loopDepth).flatMap { depth =>
        if (depth == 0) addError(s"Semantic error at ${kw.line}:${kw.column}: 'continue' outside of loop")
        else noop
      }

    case Stmt.Block(_, stmts) =>
      withScope(stmts.traverse_(analyze))

    case Stmt.Return(_, expr) => analyze(expr)
    case Stmt.ExprStmt(expr)  => analyze(expr)
    case Stmt.ErrorStmt(_)    => noop

    // ------------ EXPRESSIONS ------------
    
    case Expr.Ident(ident) => for {
      mutability <- inspect(_.symbolTable.get(ident.lexeme))
      _ <- mutability match {
        case None =>
          addError(s"Semantic error at ${ident.line}:${ident.column}: '${ident.lexeme}' not found")
        case Some(_) => noop
      }
    } yield ()

    case Expr.Call(ident, args) => for {
      arityOp <- inspect(_.functions.get(ident.lexeme))
      _ <- arityOp match {
        case None =>
          addError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' not found")
        case Some(expectedArity) if expectedArity != args.length =>
          addError(s"Semantic error at ${ident.line}:${ident.column}: function '${ident.lexeme}' expects $expectedArity arguments, but ${args.length} given")
        case Some(_) =>
          noop
      }
      _ <- args.traverse_(analyze)
    } yield ()

    case Expr.Binary(left, _, right) => analyze(left) *> analyze(right)
    case Expr.Unary(_, expr)         => analyze(expr)
    case _: (Expr.Param | Expr.IntLiteral | Expr.BoolLiteral | Expr.ErrorExpr) => noop
  }
}
