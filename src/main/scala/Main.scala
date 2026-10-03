import java.io.{File, PrintWriter}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.immutable.List


@main def main(args: String*): Unit = {
  var inputPath = ""
  var tokensOut = ""
  var astOut = ""
  var llvmOut = ""

  var i = 0
  while (i < args.length) {
    args(i) match {
      case "-t" =>
        tokensOut = args(i + 1)
        i += 2
      case "-a" =>
        astOut = args(i + 1)
        i += 2
      case "-o" =>
        llvmOut = args(i + 1)
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
    println("Usage: splc -t <tokens_out> -a <ast_out> -o <llvm_out> <input>")
    sys.exit(1)
  }

  Using(Source.fromFile(inputPath)) { source =>
    val chars = source.to(LazyList)

    // ------------ LEXER ------------
    val lexedTokens = Lexer.lexing(chars, 1, 1, Nil)
    if (tokensOut.nonEmpty) {
      Using(PrintWriter(File(tokensOut))) { writer =>
        writer.write(lexedTokens.mkString("[\n", ",\n", "\n]"))
      }
      if (lexedTokens.exists(_.isInstanceOf[Token.ErrorTok])) sys.exit(1)
    }

    if (astOut.isEmpty && llvmOut.isEmpty) {
      sys.exit(0)
    }


    // ------------ PARSER ------------
    val (parsedAst, finalParserState) = parseProgram(ParserState(lexedTokens, errors = List.empty), Nil)
    if (astOut.nonEmpty) {
      val astJsonString = AstSerializer.toJson(parsedAst)
      Using(PrintWriter(File(astOut))) { writer =>
        writer.write(astJsonString)
      }
    }

    if (finalParserState.errors.nonEmpty) {
      finalParserState.errors.reverse.foreach(System.err.println)
      sys.exit(1)
    }

    if (!parsedAst.lastOption.exists(_.isInstanceOf[Stmt.Return])) {
      System.err.println("Semantic error: last statement must be a 'return' statement")
      sys.exit(1)
    }

    // ------------ AST ANALYZER ------------
    val initialState = AstAnalyzer.AnalyzerState(symbolTable = Map.empty, errors = List.empty)
    val finalAnalyzerState = parsedAst.foldLeft(initialState)((state, stmt) => AstAnalyzer.analyze(state, stmt))

    if (finalAnalyzerState.errors.nonEmpty) {
      finalAnalyzerState.errors.reverse.foreach(System.err.println)
      sys.exit(1)
    }

    // ------------ IR GENERATOR ------------
    if (llvmOut.nonEmpty) {
      val instructions = IRGenerator.generate(parsedAst)
      val header =
        """|; ModuleID = 'spl'
           |source_filename = "spl"
           |target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
           |target triple = "x86_64-unknown-linux-gnu"
           |
           |define i64 @main() {
           |entry:""".stripMargin
      val body = instructions.mkString("\n")
      val llvmIR = s"$header\n$body\n}\n"

      Using(PrintWriter(File(llvmOut))) { writer =>
        writer.write(llvmIR)
      }
    }
  }
}
