import scala.annotation.tailrec

object Lexer {
  private val keywords: Map[String, (Int, Int) => Token] = Map(
    "val" -> Token.Val.apply,
    "var" -> Token.Var.apply,
    "return" -> Token.Return.apply,
    "if" -> Token.If.apply,
    "else" -> Token.Else.apply,
    "while" -> Token.While.apply,
    "break" -> Token.Break.apply,
    "continue" -> Token.Continue.apply,
    "true" -> Token.True.apply,
    "false" -> Token.False.apply
  )

  private val twoCharTokens: Map[(Char, Char), (Int, Int) => Token] = Map(
    ('=', '=') -> Token.Eq.apply,
    ('!', '=') -> Token.Ne.apply,
    ('<', '=') -> Token.Le.apply,
    ('>', '=') -> Token.Ge.apply,
    ('&', '&') -> Token.And.apply,
    ('|', '|') -> Token.Or.apply
  )

  private val oneCharTokens: Map[Char, (Int, Int) => Token] = Map(
    '=' -> Token.Assign.apply,
    ';' -> Token.Semi.apply,
    '+' -> Token.Plus.apply,
    '-' -> Token.Minus.apply,
    '*' -> Token.Mult.apply,
    '/' -> Token.Div.apply,
    '(' -> Token.Lparen.apply,
    ')' -> Token.Rparen.apply,
    '{' -> Token.Lbrace.apply,
    '}' -> Token.Rbrace.apply,
    '<' -> Token.Lt.apply,
    '>' -> Token.Gt.apply,
    '!' -> Token.Not.apply
  )


  @tailrec
  def lexing(chars: LazyList[Char], line: Int, column: Int, tokens: List[Token]): List[Token] = chars match {
    case '/' #:: '/' #:: tail =>
      lexing(chars.dropWhile(c => c != '\n'), line, 1, tokens)

    case '/' #:: '*' #:: tail =>
      val (nextChars, nextLine, nextColumn, nextTokens) = skipBlockComment(tail, line, column + 2, line, column, tokens)
      lexing(nextChars, nextLine, nextColumn, nextTokens)

    case c1 #:: c2 #:: tail if twoCharTokens.contains((c1, c2)) =>
      val token = twoCharTokens((c1, c2))(line, column)
      lexing(tail, line, column + 2, token :: tokens)

    case char #:: tail =>
      val (nextChars, nextLine, nextColumn, nextTokens) = char match {
        case '\n' => (tail, line + 1, 1, tokens)

        case c if c.isWhitespace => (tail, line, column + 1, tokens)

        case c if oneCharTokens.contains(c) =>
          val token = oneCharTokens(c)(line, column)
          (tail, line, column + 1, token :: tokens)

        case c if c.isDigit =>
          val (digits, rest) = chars.span(_.isDigit)
          val value = digits.mkString
          val newToken = Token.IntTok(value, line, column)
          (rest, line, column + value.length, newToken :: tokens)

        case c if (c.isLetter || c == '_') =>
          val (ident, rest) = chars.span(c => c.isLetterOrDigit || c == '_')
          val value = ident.mkString
          val newToken = keywords.get(value) match {
            case Some(constructor) => constructor(line, column)
            case _ => Token.Ident(value, line, column)
          }
          (rest, line, column + value.length, newToken :: tokens)

        case unexpected =>
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
        val errorToken = Token.ErrorTok("unterminated block comment", startLine, startColumn)
        (chars, line, column, errorToken :: tokens)
  }
}
