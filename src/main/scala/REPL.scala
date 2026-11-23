import Stmt.ExprStmt
import Token.ErrorTok

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.io.StdIn
import scala.sys.process._

object REPL {
  private case class ReplState(
    historyStmts: List[Stmt] = Nil,
    lineNumber: Int = 1
  )

  def start(): Unit =
    loop(ReplState())

  @tailrec
  private def loop(state: ReplState): Unit = {
    val str = StdIn.readLine()
    if (str != null) {
      eval(str, state) match {
        case Left(errors) =>
          errors.foreach(println)
          loop(state)
        case Right((nextState, output)) =>
          output.foreach(println)
          loop(nextState)
      }
    }
  }

  private def eval(str: String, state: ReplState): Either[List[String], (ReplState, Option[String])] = {
    for {
      tokens            <- lexLine(str, state.lineNumber)
      ast               <- parseLine(tokens)
      reachableAst       = fromASTheadToReturn(ast)
      _                 <- analyzeLine(state.historyStmts, reachableAst)
      result            <- execute(reachableAst, state)
    } yield result
  }

  private def lexLine(str: String, lineNumber: Int): Either[List[String], List[Token]] = {
    val tokens = Lexer.lexing(LazyList.from(str), lineNumber, 1, tokens = Nil)
    val errors = tokens.collect {
      case ErrorTok(msg, line, col) => s"Lexer error at $line:$col: $msg"
    }
    Either.cond(errors.isEmpty, tokens, errors)
  }

  private def parseLine(tokens: List[Token]): Either[List[String], List[Stmt]] = {
    val (stmts, errors) = Parser.parseStmts(tokens)
    Either.cond(errors.isEmpty, stmts, errors)
  }
  
  private def analyzeLine(history: List[Stmt], newStmts: List[Stmt]): Either[List[String], Unit] = {
    val errors = AstAnalyzer.analyzeStmts(history ++ newStmts)
    Either.cond(errors.isEmpty, (), errors)
  }

  private def fromASTheadToReturn(ast: List[Stmt]): List[Stmt] = {
    val (beforeReturn, fromReturn) = ast.span {
      case _: Stmt.Return => false
      case _ => true
    }
    beforeReturn ++ fromReturn.headOption
  }

  private def execute(ast: List[Stmt], state: ReplState): Either[List[String], (ReplState, Option[String])] = {
    val newStmts = ast.filter {
      case _: Stmt.Decl | _: Stmt.Assign => true
      case _                             => false
    }
    val nextHistory = state.historyStmts ++ newStmts
    val nextState   = state.copy(historyStmts = nextHistory, lineNumber = state.lineNumber + 1)

    ast.lastOption match {
      case None =>
        Right((nextState, None))

      case Some(Stmt.ExprStmt(expr)) =>
        val retStmt = Stmt.Return(Token.Return(expr.line, expr.column), expr)
        runLli(nextHistory :+ retStmt).map(out => (nextState, Some(out)))

      case Some(ret: Stmt.Return) =>
        runLli(nextHistory :+ ret).map(out => (nextState, Some(out)))

      case Some(_: Stmt.Decl | _: Stmt.Assign) =>
        Right((nextState, None))

      case Some(Stmt.ErrorStmt(token)) =>
        Left(List(s"Error at ${token.line}:${token.column}"))
        
      case Some(Stmt.If(_, _, _, _)) => ???
      case Some(Stmt.While(_, _, _)) => ???
      case Some(Stmt.Break(_)) => ???
      case Some(Stmt.Continue(_)) => ???
      case Some(Stmt.Block(_, _)) => ???
    }
  }

  private def runLli(stmts: List[Stmt]): Either[List[String], String] = {
    val main = TopDecl.FuncDecl(
      Token.Def(1, 1),
      Token.Ident("main", 1, 1),
      Nil,
      Stmt.Block(Token.Lbrace(1, 1), stmts)
    )
    val llvmIR = IRGenerator.codeGenProgram(List(main))

    val input = new ByteArrayInputStream(llvmIR.getBytes(StandardCharsets.UTF_8))
    val lliExitCode = ("lli" #< input).!
    Right(lliExitCode.toString)
  }
}
