package org.willena.slox

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox:

  // TODO ew global mutable state
  var hadError        = false
  var hadRuntimeError = false

  def error(in: Token | Int, message: String) =
    val (line, location) = in match
      case t: Token => (t.line, if t.tpe == TokenType.Eof then "end" else s"'${t.lexeme}'")
      case l: Int   => (l, "")
    Console.err.println(s"[line $line] Error at $location: $message") // GAIN no 'report' helper

  @main def run(path: String) =
    val bytes = Files.readAllBytes(Paths.get(path))
    exec(String(bytes, Charset.defaultCharset()))

  private def exec(source: String) =
    val tokens = Scanner(source).scanTokens
