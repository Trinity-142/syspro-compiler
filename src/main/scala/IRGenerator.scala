import cats.data.{Chain, State}
import cats.syntax.all.*
import scala.util.Try

object IRGenerator {
  private case class CodegenState(
    nextRegId: Int = 1,
    nextLabelId: Int = 1,
    currentLabel: String = "entry",
    identToMem: Map[String, String] = Map.empty,
    loopStack: List[(String, String)] = List.empty,
    instructions: Chain[String] = Chain.empty
  )

  private type Codegen[A] = State[CodegenState, A]

  private val noop: Codegen[Unit] = State.pure(())

  private def pure[A](value: A): Codegen[A] = State.pure(value)

  private def inspect[A](f: CodegenState => A): Codegen[A] = State.inspect(f)
  
  private def modify(f: CodegenState => CodegenState): Codegen[Unit] = State.modify(f)

  // ------------ UTILITIES ------------

  private def emitInstr(instr: String): Codegen[Unit] =
    modify(s => s.copy(instructions = s.instructions.append(instr)))

  private def emitLabel(label: String): Codegen[Unit] =
    modify(s => s.copy(
      instructions = s.instructions.append(s"$label:"),
      currentLabel = label
    ))

  private def getNextReg(prefix: String = ""): Codegen[String] = State { s =>
    val name = if (prefix.nonEmpty) s"%$prefix.${s.nextRegId}" else s"%${s.nextRegId}"
    (s.copy(nextRegId = s.nextRegId + 1), name)
  }

  private def getNextLabel(prefix: String): Codegen[String] = State { s =>
    (s.copy(nextLabelId = s.nextLabelId + 1), s"$prefix.${s.nextLabelId}")
  }

  private val getCurrentLabel: Codegen[String] = inspect(_.currentLabel)

  private def withScope[A](action: Codegen[A]): Codegen[A] = for {
    saved <- inspect(_.identToMem)
    res   <- action
    _     <- modify(_.copy(identToMem = saved))
  } yield res

  private def withLoop[A](condLabel: String, endLabel: String)(action: Codegen[A]): Codegen[A] = for {
    _   <- modify(s => s.copy(loopStack = (condLabel, endLabel) :: s.loopStack))
    res <- action
    _   <- modify(s => s.copy(loopStack = s.loopStack.tail))
  } yield res

  private def emitBrAndLabel(targetLabel: String, nextLabel: String): Codegen[Unit] = for {
    lastLine <- inspect(_.instructions.lastOption.map(_.trim).getOrElse(""))
    _ <- if (lastLine.startsWith("ret ") || lastLine.startsWith("br ")) {
      emitLabel(nextLabel)
    } else {
      emitInstr(s"  br label %$targetLabel") *> emitLabel(nextLabel)
    }
  } yield ()

  private def getIdentPtr(name: String): Codegen[String] =
    inspect(_.identToMem(name))

  private def addIdentPtr(name: String, ptrReg: String): Codegen[Unit] =
    modify(s => s.copy(identToMem = s.identToMem + (name -> ptrReg)))

  private def genCond(expr: Expr): Codegen[String] = for {
    value   <- genExpr(expr)
    condReg <- getNextReg("logictmp")
    _       <- emitInstr(s"  $condReg = icmp ne i64 $value, 0")
  } yield condReg

  // ------------ ENTRY POINTS ------------

  def codeGenProgram(decls: List[TopDecl]): String = {
    val header =
      """|; ModuleID = 'spl'
         |source_filename = "spl"
         |target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
         |target triple = "x86_64-unknown-linux-gnu"""".stripMargin

    val declsIR = decls
      .map(codeGenDecl)
      .filter(_.nonEmpty)
      .map(_.mkString("\n"))
      .mkString("\n\n")

    s"$header\n\n$declsIR\n"
  }

  // ------------ TOP-LEVEL DECLARATIONS ------------
  
  private def codeGenDecl(decl: TopDecl): List[String] = decl match {
    case TopDecl.ExternDecl(_, ident, params) =>
      val paramTypes = params.map(_ => "i64").mkString(", ")
      List(s"declare i64 @${ident.lexeme}($paramTypes)")

    case TopDecl.FuncDecl(_, ident, params, body) =>
      val paramSig = params.map(p => s"i64 %${p.ident.lexeme}").mkString(", ")

      val funcAction: Codegen[Unit] = for {
        _ <- params.traverse_ { case Expr.Param(pIdent) =>
          for {
            ptrReg <- getNextReg(pIdent.lexeme)
            _      <- emitInstr(s"  $ptrReg = alloca i64, align 8")
            _      <- emitInstr(s"  store i64 %${pIdent.lexeme}, ptr $ptrReg, align 8")
            _      <- addIdentPtr(pIdent.lexeme, ptrReg)
          } yield ()
        }
        _        <- body.stmts.traverse_(genStmt)
        lastLine <- inspect(_.instructions.lastOption.map(_.trim).getOrElse(""))
        _        <- if (!lastLine.startsWith("ret ") && !lastLine.startsWith("br ")) {
          emitInstr("  unreachable")
        } else noop
        _        <- emitInstr("}")
      } yield ()

      val bodyInstrs = funcAction.runS(CodegenState()).value.instructions.toList
      s"define i64 @${ident.lexeme}($paramSig) {" :: "entry:" :: bodyInstrs

    case TopDecl.ErrorDecl(_) => Nil
  }

  // ------------ EXPRESSIONS ------------

  private def genExpr(expr: Expr): Codegen[String] = expr match {
    case Expr.Binary(left, _: Token.And, right) => for {
      lhsCond     <- genCond(left)
      lhsLabelEnd <- getCurrentLabel
      rhsLabel    <- getNextLabel("and.rhs")
      endLabel    <- getNextLabel("and.end")
      _           <- emitInstr(s"  br i1 $lhsCond, label %$rhsLabel, label %$endLabel")
      _           <- emitLabel(rhsLabel)

      rhsCond     <- genCond(right)
      rhsLabelEnd <- getCurrentLabel
      tmp         <- getNextReg("phitmp")
      resReg      <- getNextReg("phizext")
      _           <- emitInstr(s"  br label %$endLabel")
      _           <- emitLabel(endLabel)
      _           <- emitInstr(s"  $tmp = phi i1 [ false, %$lhsLabelEnd ], [ $rhsCond, %$rhsLabelEnd ]")
      _           <- emitInstr(s"  $resReg = zext i1 $tmp to i64")
    } yield resReg

    case Expr.Binary(left, _: Token.Or, right) => for {
      lhsCond     <- genCond(left)
      lhsLabelEnd <- getCurrentLabel
      rhsLabel    <- getNextLabel("or.rhs")
      endLabel    <- getNextLabel("or.end")
      _           <- emitInstr(s"  br i1 $lhsCond, label %$endLabel, label %$rhsLabel")
      _           <- emitLabel(rhsLabel)

      rhsCond     <- genCond(right)
      rhsLabelEnd <- getCurrentLabel
      tmp         <- getNextReg("phitmp")
      resReg      <- getNextReg("phizext")
      _           <- emitInstr(s"  br label %$endLabel")
      _           <- emitLabel(endLabel)
      _           <- emitInstr(s"  $tmp = phi i1 [ true, %$lhsLabelEnd ], [ $rhsCond, %$rhsLabelEnd ]")
      _           <- emitInstr(s"  $resReg = zext i1 $tmp to i64")
    } yield resReg

    case Expr.Binary(left, op, right) => for {
      lhs <- genExpr(left)
      rhs <- genExpr(right)
      res <- tryFoldConstants(lhs, op, rhs) match {
        case Some(folded) => pure(folded)
        case None => op match {
          case _: (Token.Plus | Token.Minus | Token.Mult | Token.Div) =>
            val irOp = op match {
              case _: Token.Plus  => "add"
              case _: Token.Minus => "sub"
              case _: Token.Div   => "sdiv"
              case _: Token.Mult  => "mul"
              case _              => ???
            }
            for {
              resReg <- getNextReg(irOp)
              _      <- emitInstr(s"  $resReg = $irOp i64 $lhs, $rhs")
            } yield resReg

          case _: (Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.Le | Token.Ge) =>
            val cmpPred = op match {
              case _: Token.Eq => "eq"
              case _: Token.Ne => "ne"
              case _: Token.Lt => "slt"
              case _: Token.Gt => "sgt"
              case _: Token.Le => "sle"
              case _: Token.Ge => "sge"
              case _           => ???
            }
            for {
              tmp    <- getNextReg(s"${cmpPred}tmp")
              resReg <- getNextReg(s"${cmpPred}zext")
              _      <- emitInstr(s"  $tmp = icmp $cmpPred i64 $lhs, $rhs")
              _      <- emitInstr(s"  $resReg = zext i1 $tmp to i64")
            } yield resReg

          case _ => ???
        }
      }
    } yield res

    case Expr.IntLiteral(value, _) =>
      pure(value.toString)

    case Expr.BoolLiteral(value, _) =>
      pure(if (value) "1" else "0")

    case Expr.Ident(ident) => for {
      ptrReg <- getIdentPtr(ident.lexeme)
      resReg <- getNextReg(ident.lexeme)
      _      <- emitInstr(s"  $resReg = load i64, ptr $ptrReg, align 8")
    } yield resReg

    case Expr.Call(ident, args) => for {
      argRegs <- args.traverse(genExpr)
      resReg  <- getNextReg(s"call.${ident.lexeme}")
      argsStr  = argRegs.map(r => s"i64 $r").mkString(", ")
      _       <- emitInstr(s"  $resReg = call i64 @${ident.lexeme}($argsStr)")
    } yield resReg

    case Expr.Unary(op, expr) => for {
      rhs <- genExpr(expr)
      res <- op match {
        case _: Token.Minus => rhs.toLongOption match {
          case Some(value) => pure((-value).toString)
          case None => for {
            resReg <- getNextReg("unaryMinus")
            _      <- emitInstr(s"  $resReg = sub i64 0, $rhs")
          } yield resReg
        }

        case _: Token.Not => rhs.toLongOption match {
          case Some(value) => pure(if (value == 0) "1" else "0")
          case None => for {
            tmp    <- getNextReg("notTmp")
            resReg <- getNextReg("not")
            _      <- emitInstr(s"  $tmp = icmp eq i64 $rhs, 0")
            _      <- emitInstr(s"  $resReg = zext i1 $tmp to i64")
          } yield resReg
        }

        case _ => ???
      }
    } yield res

    case _: (Expr.Param | Expr.ErrorExpr) => ???
  }

  // ------------ STATEMENTS ------------

  private def genStmt(stmt: Stmt): Codegen[Unit] = stmt match {
    case Stmt.Decl(_, ident, value) => for {
      valReg <- genExpr(value)
      ptrReg <- getNextReg(ident.lexeme)
      _      <- emitInstr(s"  $ptrReg = alloca i64, align 8")
      _      <- emitInstr(s"  store i64 $valReg, ptr $ptrReg, align 8")
      _      <- addIdentPtr(ident.lexeme, ptrReg)
    } yield ()

    case Stmt.Assign(ident, _, value) => for {
      valReg <- genExpr(value)
      ptrReg <- getIdentPtr(ident.lexeme)
      _      <- emitInstr(s"  store i64 $valReg, ptr $ptrReg, align 8")
    } yield ()

    case Stmt.Return(_, expr) => for {
      valReg <- genExpr(expr)
      _      <- emitInstr(s"  ret i64 $valReg")
    } yield ()

    case Stmt.ExprStmt(expr) =>
      genExpr(expr).void

    case Stmt.Block(_, stmts) =>
      withScope(stmts.traverse_(genStmt))

    case Stmt.If(_, cond, thenBranch, elseBranch) => for {
      condReg   <- genCond(cond)
      thenLabel <- getNextLabel("if.then")
      endLabel  <- getNextLabel("if.end")
      _ <- elseBranch match {
        case Some(elseStmt) => for {
          elseLabel <- getNextLabel("if.else")
          _         <- emitInstr(s"  br i1 $condReg, label %$thenLabel, label %$elseLabel")
          _         <- emitLabel(thenLabel)
          _         <- withScope(genStmt(thenBranch))
          _         <- emitBrAndLabel(endLabel, elseLabel)
          _         <- withScope(genStmt(elseStmt))
          _         <- emitBrAndLabel(endLabel, endLabel)
        } yield ()

        case None =>
          emitInstr(s"  br i1 $condReg, label %$thenLabel, label %$endLabel") *>
          emitLabel(thenLabel) *>
          withScope(genStmt(thenBranch)) *>
          emitBrAndLabel(endLabel, endLabel)
      }
    } yield ()

    case Stmt.While(_, cond, body) => for {
      condLabel <- getNextLabel("while.cond")
      bodyLabel <- getNextLabel("while.body")
      endLabel  <- getNextLabel("while.end")

      _       <- emitInstr(s"  br label %$condLabel")
      _       <- emitLabel(condLabel)
      condReg <- genCond(cond)
      _       <- emitInstr(s"  br i1 $condReg, label %$bodyLabel, label %$endLabel")
      _       <- emitLabel(bodyLabel)

      _       <- withScope(withLoop(condLabel, endLabel)(genStmt(body)))
      _       <- emitBrAndLabel(condLabel, endLabel)
    } yield ()

    case Stmt.Break(_) => for {
      endLabel <- inspect(_.loopStack.head._2)
      _        <- emitInstr(s"  br label %$endLabel")
    } yield ()

    case Stmt.Continue(_) => for {
      condLabel <- inspect(_.loopStack.head._1)
      _         <- emitInstr(s"  br label %$condLabel")
    } yield ()

    case Stmt.ErrorStmt(_) => ???
  }

  private def tryFoldConstants(lhs: String, op: Token, rhs: String): Option[String] = (lhs.toLongOption, rhs.toLongOption) match {
    case (Some(x), Some(y)) => op match {
      case _: Token.Plus  => Some((x + y).toString)
      case _: Token.Minus => Some((x - y).toString)
      case _: Token.Mult  => Some((x * y).toString)
      case _: Token.Div   => Try(x / y).map(_.toString).toOption
      case _: Token.Eq    => Some(if (x == y) "1" else "0")
      case _: Token.Ne    => Some(if (x != y) "1" else "0")
      case _: Token.Lt    => Some(if (x < y) "1" else "0")
      case _: Token.Gt    => Some(if (x > y) "1" else "0")
      case _: Token.Le    => Some(if (x <= y) "1" else "0")
      case _: Token.Ge    => Some(if (x >= y) "1" else "0")
      case _              => None
    }
    case _ => None
  }
}
