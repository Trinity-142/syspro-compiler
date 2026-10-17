import scala.collection.mutable.ListBuffer
import scala.util.Try

object IRGenerator {
  def generate(stmts: List[Stmt]): List[String] = {
    var nextRegId = 1
    var identToMem = Map.empty[String, String]
    val instructions = ListBuffer.empty[String]

    // ------------ UTILITIES ------------
    def getNextReg: String = {
      val regName = s"%$nextRegId"
      nextRegId += 1
      regName
    }

    def emitInstr(instr: String): Unit = {
      instructions += instr
    }

    def getIdentPtr(name: String): String = {
      identToMem(name)
    }

    def addIdentPtr(name: String, ptrReg: String): Unit = {
      identToMem += (name -> ptrReg)
    }

    // ------------ EXPRESSIONS ------------
    def genExpr(expr: Expr): String = expr match {
      case Expr.Binary(left, op, right) =>
        val lhs = genExpr(left)
        val rhs = genExpr(right)
        val resReg = getNextReg

        tryFoldConstants(lhs, op, rhs) match {
          case Some(str) => str
          case _ => {
            val irOp = op match {
              case _: Token.Plus => "add"
              case _: Token.Minus => "sub"
              case _: Token.Div => "sdiv"
              case _: Token.Mult => "mul"
              case _ => ???
            }
            emitInstr(s"  $resReg = $irOp i64 $lhs, $rhs")
            resReg
          }
        }

      case Expr.IntLiteral(value, _) =>
        value.toString

      case Expr.Ident(ident) =>
        val ptrReg = getIdentPtr(ident.lexeme)
        val resReg = getNextReg
        emitInstr(s"  $resReg = load i64, ptr $ptrReg, align 8")
        resReg

      case Expr.Unary(op, expr) =>
        val rhs = genExpr(expr)
        val resReg = getNextReg

        val irOp = op match {
          case _: Token.Minus => "sub"
          case _ => ???
        }

        emitInstr(s"  $resReg = sub i64 0, $rhs")
        resReg

      case Expr.ErrorExpr(_) => ???
    }

    // ------------ STATEMENTS ------------
    def genStmt(stmt: Stmt): Unit = stmt match {
      case Stmt.Decl(kw, ident, value) =>
        val valReg = genExpr(value)
        val ptrReg = getNextReg
        emitInstr(s"  $ptrReg = alloca i64, align 8")
        emitInstr(s"  store i64 $valReg, ptr $ptrReg, align 8")
        addIdentPtr(ident.lexeme, ptrReg)

      case Stmt.Assign(ident, value) =>
        val valReg = genExpr(value)
        val ptrReg = getIdentPtr(ident.lexeme)
        emitInstr(s"  store i64 $valReg, ptr $ptrReg, align 8")

      case Stmt.Return(kw, expr) =>
        val valReg = genExpr(expr)
        emitInstr(s"  ret i64 $valReg")

      case Stmt.ExprStmt(expr) =>
        genExpr(expr)

      case Stmt.ErrorStmt(_) => ???
    }

    stmts.foreach(genStmt)
    instructions.toList
  }

  private def tryFoldConstants(lhs: String, op: Token, rhs: String): Option[String] = (lhs.toLongOption, rhs.toLongOption) match {
    case (Some(x), Some(y)) => op match {
      case _: Token.Plus => Some((x + y).toString)
      case _: Token.Minus => Some((x - y).toString)
      case _: Token.Mult => Some((x * y).toString)
      case _: Token.Div => Try(x / y).map(_.toString).toOption
      case _ => None
    }
    case _ => None
  }
}
