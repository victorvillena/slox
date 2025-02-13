package org.willena.slox

import scala.collection.mutable.ListBuffer

class Scanner(val source: String):

  // State /////////////////////////////////////////////////////////////////////

  private var currentLine = 1

  private var currentCharIndex = 0
  private var tokenStartIndex  = 0

  private val tokens = ListBuffer[Token]()

  // Helpers ///////////////////////////////////////////////////////////////////

  private def endOfSource(offset: Int = 0) = currentCharIndex + offset >= source.length

  /** Return current char and move the index forward */
  private def advance =
    currentCharIndex += 1
    source.charAt(currentCharIndex - 1)

  private def peek(offset: Int = 0) = if endOfSource(offset) then '\u0000' else source.charAt(currentCharIndex + offset)

  /** Return true and move the index forward if the current char matches the argument.
    */
  private def matches(expected: Char) =
    if endOfSource() then false
    else if source.charAt(currentCharIndex) != expected then false
    else
      currentCharIndex += 1
      true

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlpha(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'

  private def isAlphanumeric(c: Char) = isDigit(c) || isAlpha(c)

  private def addToken(tpe: TokenType, literal: LiteralValue = null) = // GAIN default params avoid overloading
    val text = source.substring(tokenStartIndex, currentCharIndex)
    tokens += Token(tpe, text, literal, currentLine)

  //////////////////////////////////////////////////////////////////////////////

  def scanTokens: Seq[Token] =
    while !endOfSource() do
      tokenStartIndex = currentCharIndex
      scanToken()

    tokens += Token(TokenType.Eof, "", null, currentLine) // TODO can Token and TokenType meld into an ADT?
    tokens.toList

  // TODO idea: this could return either a tokenAdd, or an error. Check if this is the case.
  private def scanToken(): Unit =
    import TokenType.*
    advance match
      case '(' => addToken(LeftParen)
      case ')' => addToken(RightParen)
      case '{' => addToken(LeftBrace)
      case '}' => addToken(RightBrace)
      case ',' => addToken(Comma)
      case '.' => addToken(Dot)
      case '-' => addToken(Minus)
      case '+' => addToken(Plus)
      case ';' => addToken(Semicolon)
      case '*' => addToken(Star)

      case '!' => addToken(if matches('=') then BangEqual else Bang)
      case '=' => addToken(if matches('=') then EqualEqual else Equal)
      case '<' => addToken(if matches('=') then LessEqual else Less)
      case '>' => addToken(if matches('=') then GreaterEqual else Greater)

      case '/' =>
        if matches('/') then while peek() != '\n' && !endOfSource() do advance
        else addToken(Slash)

      case ' ' | '\r' | '\t' => () // ignore whitespace

      case '\n' => currentLine += 1

      case '"' =>
        while peek() != '"' && !endOfSource() do
          if peek() == '\n' then
            currentLine += 1 // multiline strings are valid, but the line counter needs to be updated
          advance
        if endOfSource() then return Lox.error(currentLine, "Unterminated string.")
        advance // consume the closing " after peeking it
        addToken(String, source.substring(tokenStartIndex + 1, currentCharIndex - 1)) // string, without quotes

      case d if isDigit(d) =>
        while isDigit(peek()) do advance          // consume digits
        if peek() == '.' && isDigit(peek(1)) then // decimal point!
          advance                                 // consume decimal point
          while isDigit(peek()) do advance        // and consume the decimal digits

        addToken(Number, source.substring(tokenStartIndex, currentCharIndex).toDouble)

      case a if isAlpha(a) =>
        while isAlphanumeric(peek()) do advance // consume entire identifier
        val text = source.substring(tokenStartIndex, currentCharIndex)
        val tpe = text match
          case "and"    => And
          case "class"  => Class
          case "else"   => Else
          case "false"  => False
          case "for"    => For
          case "fun"    => Fun
          case "if"     => If
          case "nil"    => Nil
          case "or"     => Or
          case "print"  => Print
          case "return" => Return
          case "super"  => Super
          case "this"   => This
          case "true"   => True
          case "var"    => Var
          case "while"  => While
          case _        => Identifier
        addToken(tpe)

      case _ => Lox.error(currentLine, "Unexpected character.")

end Scanner
