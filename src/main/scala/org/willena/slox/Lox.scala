package org.willena.slox

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox:

  private var hadError        = false
  private var hadRuntimeError = false

  def error(in: Token | Int, message: String) =
    val (line, location) = in match
      case t: Token =>
        val where = if t.tpe == TokenType.Eof then "end" else s"'${t.lexeme}'"
        (t.line, s" at $where")
      case l: Int => (l, "")
    Console.err.println(s"[line $line] Error$location: $message")
    hadError = true

  def runtimeError(error: RuntimeError) =
    Console.err.println(s"${error.getMessage}\n[line ${error.token.line}]")
    hadRuntimeError = true

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
