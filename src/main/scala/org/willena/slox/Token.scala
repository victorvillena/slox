package org.willena.slox

type LiteralValue = Double | String | Boolean | Null

class Token(val tpe: TokenType, val lexeme: String, val literal: LiteralValue, val line: Int):

  override def toString: String = s"$tpe $lexeme $literal"
