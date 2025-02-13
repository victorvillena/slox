package org.willena.slox

class Token(val tpe: TokenType, val lexeme: String, val literal: Any, val line: Int):

  override def toString: String = s"$tpe $lexeme $literal"
