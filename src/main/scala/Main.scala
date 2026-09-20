import java.io.{File, PrintWriter}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.immutable.List


@main def main(args: String*): Unit = {
  var inputPath = ""
  var tokensOut = ""
  var astOut = ""

  var i = 0
  while (i < args.length) {
    args(i) match {
      case "-t" =>
        tokensOut = args(i + 1)
        i += 2
      case "-a" =>
        astOut = args(i + 1)
        i += 2
      case arg if !arg.startsWith("-") =>
        inputPath = arg
        i += 1
      case unknown =>
        System.err.println(s"Error: unknown option '$unknown'")
        sys.exit(1)
    }
  }

  if (inputPath.isEmpty) {
    println("Usage: splc -t <tokens_out> -a <ast_out> <input>")
    sys.exit(1)
  }

  Using(Source.fromFile(inputPath)) { source =>
    val chars = source.to(LazyList)

    val lexedTokens = lexer(chars, 1, 1, Nil)
    if (tokensOut.nonEmpty) {
      Using(PrintWriter(File(tokensOut))) { writer =>
        writer.write(lexedTokens.mkString("[\n", ",\n", "\n]"))
      }
      if (lexedTokens.exists(_.isInstanceOf[Token.ErrorTok])) sys.exit(1)
    }


    if (astOut.nonEmpty) {
      val parsedAst = parseProgram(lexedTokens, Nil)
      if (astOut.nonEmpty) {
        val astJsonString = AstSerializer.toJson(parsedAst)
        Using(PrintWriter(File(astOut))) { writer =>
          writer.write(astJsonString)
        }
      }
      if (hasParserErrors) sys.exit(1)
      if (!hasReturn(parsedAst)) {
        System.err.println("Semantic error: last statement must be a 'return' statement")
        sys.exit(1)
      }

      val initialState = AnalyzerState(Map.empty, List.empty)
      val finalState = parsedAst.foldLeft(initialState)((state, stmt) => analyze(state, stmt))

      val errors = finalState.semanticErrors.reverse
      if (errors.nonEmpty) {
        errors.foreach(System.err.println)
        sys.exit(1)
      }
    }
  }
}

def hasReturn(stmts: List[Stmt]): Boolean = stmts.lastOption match {
  case Some(Stmt.Return(_, _)) => true
  case _ => false
}

@tailrec
def lexer(chars: LazyList[Char], line: Int, column: Int, tokens: List[Token]): List[Token] = {
  chars match
    case '/' #:: '/' #:: tail =>
      lexer(chars.dropWhile(c => c != '\n'), line, 1, tokens)

    case '/' #:: '*' #:: tail =>
      val (nextChars, nextLine, nextColumn, nextTokens) = skipBlockComment(tail, line, column + 2, line, column, tokens)
      lexer(nextChars, nextLine, nextColumn, nextTokens)

    case char #:: tail =>
      val (nextChars, nextLine, nextColumn, nextTokens) = char match {
        case '=' => (tail, line, column + 1, Token.Assign(line, column) :: tokens)
        case ';' => (tail, line, column + 1, Token.Semi(line, column)   :: tokens)
        case '+' => (tail, line, column + 1, Token.Plus(line, column)   :: tokens)
        case '-' => (tail, line, column + 1, Token.Minus(line, column)  :: tokens)
        case '*' => (tail, line, column + 1, Token.Mult(line, column)   :: tokens)
        case '/' => (tail, line, column + 1, Token.Div(line, column)    :: tokens)
        case '(' => (tail, line, column + 1, Token.Lparen(line, column) :: tokens)
        case ')' => (tail, line, column + 1, Token.Rparen(line, column) :: tokens)
        case '\n' => (tail, line + 1, 1, tokens)
        case c if c.isWhitespace => (tail, line, column + 1, tokens)

        case c if (c == '0' || c.isDigit && c != '0') =>
          val (digits, rest) = chars.span(_.isDigit)
          val value = digits.mkString
          val newToken = Token.IntTok(value, line, column)
          (rest, line, column + value.length, newToken :: tokens)

        case c if (c.isLetter || c == '_') =>
          val (ident, rest) = chars.span(_.isLetterOrDigit)
          val value = ident.mkString
          val newToken = value match {
            case "val"    => Token.Val(line, column)
            case "var"    => Token.Var(line, column)
            case "return" => Token.Return(line, column)
            case _ => Token.Ident(value, line, column)
          }
          (rest, line, column + value.length, newToken :: tokens)

        case unexpected =>
          // System.err.println(s"Lexer error at $line:$column: unexpected character '$unexpected'")
          val errorToken = Token.ErrorTok(s"unexpected character: '$unexpected'", line, column)
          (tail, line, column + 1, errorToken :: tokens)
      }
      lexer(nextChars, nextLine, nextColumn, nextTokens)

    case LazyList() => (Token.Eof(line, column) :: tokens).reverse
}

@tailrec
def skipBlockComment(chars: LazyList[Char], 
                     line: Int, 
                     column: Int, 
                     startLine: Int, 
                     startColumn: Int,
                     tokens: List[Token]): (LazyList[Char], Int, Int, List[Token]) = {
  chars match
    case '*' #:: '/' #:: tail =>
      (tail, line, column + 2, tokens)

    case '\n' #:: tail =>
      skipBlockComment(tail, line + 1, 1, startLine, startColumn, tokens)

    case _ #:: tail =>
      skipBlockComment(tail, line, column + 1, startLine, startColumn, tokens)

    case LazyList() =>
      // System.err.println(s"Lexer error at $startLine:$startColumn: unterminated block comment")
      val errorToken = Token.ErrorTok("unterminated block comment", startLine, startColumn)
      (chars, line, column, errorToken :: tokens)
}
