package org.willena.slox

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox:

  // TODO ew global mutable state
  private var hadError        = false
  private var hadRuntimeError = false

  def error(in: Token | Int, message: String) =
    val (line, location) = in match
      case t: Token => (t.line, if t.tpe == TokenType.Eof then "end" else s"'${t.lexeme}'")
      case l: Int   => (l, "")
    Console.err.println(s"[line $line] Error at $location: $message") // GAIN no 'report' helper
    hadError = true

  def runtimeError(error: RuntimeError) =
    Console.err.println(s"${error.getMessage} \n[line ${error.token.line}]")
    hadRuntimeError = true

  // Named this way to allow for a "slox run" command
  @main def run(path: String) =
    val bytes = Files.readAllBytes(Paths.get(path))
    exec(String(bytes, Charset.defaultCharset()))
    if hadError then sys.exit(65)
    if hadRuntimeError then sys.exit(70)

  private def exec(source: String): Unit =
    val tokens     = Scanner(source).scanTokens
    val statements = Parser(tokens).parse

    if hadError then return

    val interpreter = Interpreter()
    Resolver(interpreter).resolve(statements)

    if hadError then return

    interpreter.interpret(statements)
