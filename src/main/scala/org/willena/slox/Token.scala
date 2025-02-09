package org.willena.slox

case class Token(tpe: TokenType, lexeme: String, literal: Any, line: Int):

  override def toString: String = s"$tpe $lexeme $literal"
