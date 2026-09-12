import Kind.{DIV, EOF, EQ, ERROR, IDENT, INT, LPAREN, MINUS, MULT, PLUS, RETURN, RPAREN, SEMI, VAL, VAR}
import java.io.{File, PrintWriter}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.immutable.List

enum Kind {
  case INT, EOF, EQ, SEMI, IDENT, VAL, VAR, RETURN, PLUS, MINUS, MULT, DIV, ERROR, LPAREN, RPAREN
}

case class Token(kind: Kind, value: String, line: Int, column: Int) {
  override def toString: String = s"""{"kind": "$kind", "value": "$value", "line": $line, "column": $column}"""
}


@main def main(args: String*): Unit = {
  var inputPath = ""
  var outputPath = ""

  var i = 0
  while (i < args.length) {
    args(i) match {
      case "-t" =>
        outputPath = args(i + 1)
        i += 2
      case "-g" =>
        i += 2
      case arg if !arg.startsWith("-") =>
        inputPath = arg
        i += 1
      case _ =>
        i += 1
    }
  }
  if (inputPath.isEmpty || outputPath.isEmpty) {
    println("Usage: main -g <grammar> -t <out> <input>")
    sys.exit(1)
  }

  val tokens = List[Token]()

  Using(Source.fromFile(inputPath)) { source =>
    val chars = source.to(LazyList)
    val lexedTokens = lexer(chars, 1, 1, tokens)
    Using(PrintWriter(File(outputPath))) { writer =>
      writer.write(lexedTokens.mkString("[\n", ",\n", "\n]"))
    }
    if (lexedTokens.exists(_.kind == ERROR)) sys.exit(1)
  }
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
        case '=' => (tail, line, column + 1, Token(EQ, "=", line, column) :: tokens)
        case ';' => (tail, line, column + 1, Token(SEMI, ";", line, column) :: tokens)
        case '+' => (tail, line, column + 1, Token(PLUS, "+", line, column) :: tokens)
        case '-' => (tail, line, column + 1, Token(MINUS, "-", line, column) :: tokens)
        case '*' => (tail, line, column + 1, Token(MULT, "*", line, column) :: tokens)
        case '/' => (tail, line, column + 1, Token(DIV, "/", line, column) :: tokens)
        case '(' => (tail, line, column + 1, Token(LPAREN, "(", line, column) :: tokens)
        case ')' => (tail, line, column + 1, Token(RPAREN, ")", line, column) :: tokens)
        case '\n' => (tail, line + 1, 1, tokens)
        case c if c.isWhitespace => (tail, line, column + 1, tokens)

        case c if (c == '0' || c.isDigit && c != '0') =>
          val (digits, rest) = chars.span(_.isDigit)
          val value = digits.mkString
          val newToken = Token(INT, value, line, column)
          (rest, line, column + value.length, newToken :: tokens)

        case c if (c.isLetter || c == '_') =>
          val (ident, rest) = chars.span(_.isLetterOrDigit)
          val value = ident.mkString
          val kind = value match {
            case "val" => VAL
            case "var" => VAR
            case "return" => RETURN
            case _ => IDENT
          }
          val newToken = Token(kind, value, line, column)
          (rest, line, column + value.length, newToken :: tokens)

        case unknown =>
          System.err.println(s"Lexer error at $line:$column: Unexpected character '$unknown'")
          val errorToken = Token(ERROR, s"unexpected character: '$unknown'", line, column)
          (tail, line, column + 1, errorToken :: tokens)
      }
      lexer(nextChars, nextLine, nextColumn, nextTokens)

    case LazyList() => (Token(EOF, "", line, column) :: tokens).reverse
}

@tailrec
def skipBlockComment(
  chars: LazyList[Char],
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
      System.err.println(s"Lexer error at $startLine:$startColumn: unterminated block comment")
      val errorToken = Token(ERROR, "unterminated block comment", startLine, startColumn)
      (chars, line, column, errorToken :: tokens)
}
