import scala.annotation.tailrec

object Lexer {
  @tailrec
  def lexing(chars: LazyList[Char], line: Int, column: Int, tokens: List[Token]): List[Token] = {
    chars match
      case '/' #:: '/' #:: tail =>
        lexing(chars.dropWhile(c => c != '\n'), line, 1, tokens)

      case '/' #:: '*' #:: tail =>
        val (nextChars, nextLine, nextColumn, nextTokens) = skipBlockComment(tail, line, column + 2, line, column, tokens)
        lexing(nextChars, nextLine, nextColumn, nextTokens)

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
        lexing(nextChars, nextLine, nextColumn, nextTokens)

      case LazyList() => (Token.Eof(line, column) :: tokens).reverse
  }

  @tailrec
  private def skipBlockComment(chars: LazyList[Char],
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
}
