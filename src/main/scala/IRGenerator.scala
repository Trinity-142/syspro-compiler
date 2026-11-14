import scala.collection.mutable.ListBuffer
import scala.util.Try

object IRGenerator {
  def generate(stmts: List[Stmt]): List[String] = {
    var nextRegId = 1
    var nextLabelId = 1
    var identToMem = Map.empty[String, String]
    var loopStack = List.empty[(String, String)]
    val instructions = ListBuffer.empty[String]

    // ------------ UTILITIES ------------
    def getNextReg(prefix: String = ""): String = {
      val regName = if (prefix.nonEmpty) s"%$prefix.$nextRegId" else s"$nextRegId"
      nextRegId += 1
      regName
    }

    def getNextLabel(prefix: String): String = {
      val labelName = s"$prefix.$nextLabelId"
      nextLabelId += 1
      labelName
    }

    def emitInstr(instr: String): Unit = {
      instructions += instr
    }

    def emitBrAndLabel(targetLabel: String, nextLabel: String): Unit = {
      val lastLine = instructions.lastOption.map(_.split('\n').last.trim).getOrElse("")
      if (lastLine.startsWith("ret ") || lastLine.startsWith("br ")) {
        emitInstr(s"$nextLabel:")
      } else {
        emitInstr(
          s"""  br label %$targetLabel
             |$nextLabel:""".stripMargin)
      }
    }

    def getIdentPtr(name: String): String = {
      identToMem(name)
    }

    def addIdentPtr(name: String, ptrReg: String): Unit = {
      identToMem += (name -> ptrReg)
    }

    def genCond(expr: Expr): String = {
      val value = genExpr(expr)
      val condReg = getNextReg("logictmp")
      emitInstr(s"  $condReg = icmp ne i64 $value, 0")
      condReg
    }

    def getCurrentLabel: String = {
      instructions.reverseIterator
        .flatMap(_.linesIterator.toSeq.reverseIterator)
        .find(_.endsWith(":"))
        .getOrElse("entry:")
        .stripSuffix(":")
    }

    // ------------ EXPRESSIONS ------------
    def genExpr(expr: Expr): String = expr match {
      case Expr.Binary(left, _: Token.And, right) =>
        val lhsCond = genCond(left)
        val lhsLabelEnd = getCurrentLabel
        val rhsLabel = getNextLabel("and.rhs")
        val endLabel = getNextLabel("and.end")
        emitInstr(
          s"""  br i1 $lhsCond, label %$rhsLabel, label %$endLabel
             |
             |$rhsLabel:""".stripMargin)

        val rhsCond = genCond(right)
        val rhsLabelEnd = getCurrentLabel
        val tmp = getNextReg("phitmp")
        val resReg = getNextReg("phizext")
        emitInstr(
          s"""  br label %$endLabel
             |
             |$endLabel:
             |  $tmp = phi i1 [ false, %$lhsLabelEnd ], [ $rhsCond, %$rhsLabelEnd ]
             |  $resReg = zext i1 $tmp to i64""".stripMargin)
        resReg

      case Expr.Binary(left, _: Token.Or, right) =>
        val lhsCond = genCond(left)
        val lhsLabelEnd = getCurrentLabel
        val rhsLabel = getNextLabel("or.rhs")
        val endLabel = getNextLabel("or.end")
        emitInstr(
          s"""  br i1 $lhsCond, label %$endLabel, label %$rhsLabel
             |
             |$rhsLabel:""".stripMargin)

        val rhsCond = genCond(right)
        val rhsLabelEnd = getCurrentLabel
        val tmp = getNextReg("phitmp")
        val resReg = getNextReg("phizext")
        emitInstr(
          s"""  br label %$endLabel
             |
             |$endLabel:
             |  $tmp = phi i1 [ true, %$lhsLabelEnd ], [ $rhsCond, %$rhsLabelEnd ]
             |  $resReg = zext i1 $tmp to i64""".stripMargin)
        resReg

      case Expr.Binary(left, op, right) =>
        val lhs = genExpr(left)
        val rhs = genExpr(right)

        tryFoldConstants(lhs, op, rhs) match {
          case Some(str) => str
          case _ => op match {
            case _: Token.Plus | _: Token.Minus | _: Token.Mult | _: Token.Div =>
              val irOp = op match {
                case _: Token.Plus  => "add"
                case _: Token.Minus => "sub"
                case _: Token.Div   => "sdiv"
                case _: Token.Mult  => "mul"
                case _              => ???
              }
              val resReg = getNextReg(s"$irOp")
              emitInstr(s"  $resReg = $irOp i64 $lhs, $rhs")
              resReg

            case _: Token.Eq | _: Token.Ne | _: Token.Lt | _: Token.Gt | _: Token.Le | _: Token.Ge =>
              val cmpPred = op match {
                case _: Token.Eq => "eq"
                case _: Token.Ne => "ne"
                case _: Token.Lt => "slt"
                case _: Token.Gt => "sgt"
                case _: Token.Le => "sle"
                case _: Token.Ge => "sge"
                case _           => ???
              }
              val tmp = getNextReg(s"${cmpPred}tmp")
              val resReg = getNextReg(s"${cmpPred}zext")
              emitInstr(
                s"""  $tmp = icmp $cmpPred i64 $lhs, $rhs
                   |  $resReg = zext i1 $tmp to i64""".stripMargin)
              resReg

            case _ => ???
          }
        }

      case Expr.IntLiteral(value, _) =>
        value.toString

      case Expr.BoolLiteral(value, _) =>
        if (value) "1" else "0"

      case Expr.Ident(ident) =>
        val ptrReg = getIdentPtr(ident.lexeme)
        val resReg = getNextReg(ident.lexeme)
        emitInstr(s"  $resReg = load i64, ptr $ptrReg, align 8")
        resReg

      case Expr.Unary(op, expr) =>
        val rhs = genExpr(expr)

        op match {
          case _: Token.Minus => rhs.toLongOption match {
            case Some(value) => (-value).toString
            case None =>
              val resReg = getNextReg("unaryMinus")
              emitInstr(s"  $resReg = sub i64 0, $rhs")
              resReg
          }

          case _: Token.Not => rhs.toLongOption match {
            case Some(value) => if (value == 0) "1" else "0"
            case None =>
              val tmp = getNextReg("notTmp")
              val resReg = getNextReg("not")
              emitInstr(
                s"""  $tmp = icmp eq i64 $rhs, 0
                   |  $resReg = zext i1 $tmp to i64""".stripMargin)
              resReg
          }

          case _ => ???
        }

      case Expr.ErrorExpr(_) => ???
    }

    // ------------ STATEMENTS ------------
    def genStmt(stmt: Stmt): Unit = stmt match {
      case Stmt.Decl(kw, ident, value) =>
        val valReg = genExpr(value)
        val ptrReg = getNextReg(ident.lexeme)
        emitInstr(
          s"""  $ptrReg = alloca i64, align 8
             |  store i64 $valReg, ptr $ptrReg, align 8""".stripMargin)
        addIdentPtr(ident.lexeme, ptrReg)

      case Stmt.Assign(ident, _, value) =>
        val valReg = genExpr(value)
        val ptrReg = getIdentPtr(ident.lexeme)
        emitInstr(s"  store i64 $valReg, ptr $ptrReg, align 8")

      case Stmt.Return(kw, expr) =>
        val valReg = genExpr(expr)
        emitInstr(s"  ret i64 $valReg")

      case Stmt.ExprStmt(expr) =>
        genExpr(expr)

      case Stmt.Block(_, stmts) =>
        val envSnap = identToMem
        stmts.foreach(genStmt)
        identToMem = envSnap

      case Stmt.If(_, cond, thenBranch, elseBranch) =>
        val condReg = genCond(cond)
        val thenLabel = getNextLabel("if.then")
        val endLabel = getNextLabel("if.end")

        elseBranch match {
          case Some(elseStmt) =>
            val elseLabel = getNextLabel("if.else")
            emitInstr(
              s"""  br i1 $condReg, label %$thenLabel, label %$elseLabel
                 |$thenLabel:""".stripMargin)

            val envSnapThen = identToMem
            genStmt(thenBranch)
            identToMem = envSnapThen

            emitBrAndLabel(endLabel, elseLabel)

            val envSnapElse = identToMem
            genStmt(elseStmt)
            identToMem = envSnapElse

            emitBrAndLabel(endLabel, endLabel)

          case None =>
            emitInstr(
              s"""  br i1 $condReg, label %$thenLabel, label %$endLabel
                 |$thenLabel:""".stripMargin)

            val envSnapThen = identToMem
            genStmt(thenBranch)
            identToMem = envSnapThen

            emitBrAndLabel(endLabel, endLabel)
        }

      case Stmt.While(_, cond, body) =>
        val condLabel = getNextLabel("while.cond")
        val bodyLabel = getNextLabel("while.body")
        val endLabel = getNextLabel("while.end")

        emitInstr(
          s"""  br label %$condLabel
             |$condLabel:""".stripMargin)

        val condReg = genCond(cond)
        emitInstr(
          s"""  br i1 $condReg, label %$bodyLabel, label %$endLabel
             |$bodyLabel:""".stripMargin)

        val savedEnv = identToMem
        loopStack = (condLabel, endLabel) :: loopStack
        genStmt(body)
        loopStack = loopStack.tail
        identToMem = savedEnv

        emitBrAndLabel(condLabel, endLabel)

      case Stmt.Break(_) =>
        val (_, endLabel) = loopStack.head
        emitInstr(s"  br label %$endLabel")

      case Stmt.Continue(_) =>
        val (condLabel, _) = loopStack.head
        emitInstr(s"  br label %$condLabel")

      case Stmt.ErrorStmt(_) => ???
    }

    stmts.foreach(genStmt)
    instructions.toList
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