import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Using
import scala.collection.immutable.List
import scala.sys.process.*


@main def main(args: String*): Unit = {
  if (args.isEmpty) {
    REPL.start()
    return
  }
  var inputPath = ""
  var tokensOut = ""
  var astOut = ""
  var llvmOut = ""

  var i = 0
  while (i < args.length) {
    args(i) match {
      case "--help" =>
        println("Usage: splc -t <tokens_out> -a <ast_out> -o <llvm_out> <input>")
        println("REPL: splc")
        i += 1
      case "-t" =>
        tokensOut = args(i + 1)
        i += 2
      case "-a" =>
        astOut = args(i + 1)
        i += 2
      case "-o" =>
        llvmOut = args(i + 1)
        i += 2
      case "-g" =>
        i += 2
      case arg if !arg.startsWith("-") =>
        inputPath = arg
        i += 1
      case unknown =>
        System.err.println(s"Error: unknown option '$unknown'")
        sys.exit(1)
    }
  }

  if (inputPath.isEmpty && !args.contains("--help")) {
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
    val (parsedAst, parserErrors) = parseProgram(ParserState(lexedTokens))
    if (astOut.nonEmpty) {
      val astJsonString = AstSerializer.toJson(parsedAst)
      Using(PrintWriter(File(astOut))) { writer =>
        writer.write(astJsonString)
      }
    }

    if (parserErrors.nonEmpty) {
      parserErrors.foreach(System.err.println)
      sys.exit(1)
    }

    // ------------ AST ANALYZER ------------
    val finalAnalyzerState = AstAnalyzer.analyzeProgram(parsedAst)

    if (finalAnalyzerState.errors.nonEmpty) {
      finalAnalyzerState.errors.reverse.foreach(System.err.println)
      sys.exit(1)
    }

    // ------------ IR GENERATOR & COMPILATION ------------
    if (llvmOut.nonEmpty) {
      val llvmIR = IRGenerator.codeGenProgram(parsedAst)

      if (llvmOut.endsWith(".ll")) {
        Using(PrintWriter(File(llvmOut))) { writer =>
          writer.write(llvmIR)
        }
      } else {
        val tmpllFile = File.createTempFile("prog", ".ll")
        tmpllFile.deleteOnExit()
        Using(PrintWriter(tmpllFile)) { writer =>
          writer.write(llvmIR)
        }

        val projectRoot = File(IRGenerator.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getParentFile.getParentFile
        val runtimeLibDir = File(projectRoot, "libruntime/build").getAbsolutePath

        val compileCommand = s"clang -Wno-override-module ${tmpllFile.getAbsolutePath} -L$runtimeLibDir -lsplruntime -o $llvmOut"
        val compileExitCode = compileCommand.!
        if (compileExitCode != 0) {
          System.err.println("LLVM IR compilation error")
          sys.exit(1)
        }
      }
    }
  }
}