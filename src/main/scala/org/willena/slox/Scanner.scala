package org.willena.slox

import scala.collection.mutable.ListBuffer

class Scanner(val source: String):

  private var currentLine = 0

  private var currentCharIndex = 0
  private var tokenStartIndex  = 0

  private val tokens = ListBuffer[Token]()

  def scanTokens: Seq[Token] =
    while !endOfSource do
      tokenStartIndex = currentCharIndex
      scanToken

    tokens += Token(TokenType.Eof, "", null, currentLine) // TODO can Token and TokenType meld into an ADT?
    tokens.toList

  private def scanToken = ???

  private def endOfSource = currentCharIndex >= source.length
