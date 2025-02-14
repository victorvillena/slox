package org.willena.slox

import scala.collection.mutable.ListBuffer

import org.willena.slox
import org.willena.slox.Expression as e
import org.willena.slox.Statement as s
import org.willena.slox.TokenType.*

class ParseError extends RuntimeException

class Parser(val tokens: Seq[Token]):

  private var current = 0

  // Helpers ///////////////////////////////////////////////////////////////////

  private def peek     = tokens(current)
  private def previous = tokens(current - 1)
  private def atEnd    = peek.tpe == Eof
  private def advance =
    if !atEnd then current += 1
    previous

  private def check(tpe: TokenType) = !atEnd && peek.tpe == tpe

  private def matches(types: TokenType*) =
    if types.exists(check) then
      advance
      true
    else false

  private def error(token: Token, message: String) =
    Lox.error(token, message)
    ParseError()

  private def consume(tpe: TokenType, message: String) =
    if check(tpe) then advance else throw error(peek, message)

  private def synchronize(): Unit =
    advance
    while !atEnd do
      previous.tpe match
        case Semicolon => return
        case _         => ()

      peek.tpe match
        case Class | Fun | Var | For | If | While | Print | Return => return
        case _                                                     => ()

      advance

  // general grammar ////////////////////////////////////////////////////////////

  def parse =
    val statements = ListBuffer[Statement]()
    while !atEnd do statements += declaration
    statements.toList

  private def declaration: Statement =
    try
      if matches(Class) then classDeclaration
      else if matches(Fun) then function("function")
      else if matches(Var) then varDeclaration
      else statement
    catch
      case error: ParseError =>
        synchronize()
        null

  private def statement: Statement =
    if matches(For) then forStatement
    else if matches(If) then ifStatement
    else if matches(Print) then printStatement
    else if matches(Return) then returnStatement
    else if matches(While) then whileStatement
    else if matches(LeftBrace) then s.Block(block)
    else expressionStatement

  private def expressionStatement =
    val value = expression
    consume(Semicolon, "Expect ';' after expression.")
    s.ExpressionStatement(value)

  private def expression = assignment

  private def assignment: e =
    val expr = or

    if matches(Equal) then
      val equals = previous
      val value  = assignment

      expr match
        case e.Variable(name) => e.Assign(name, value)
        case e.Get(obj, name) => e.Set(obj, name, value)
        case _ =>
          error(equals, "Invalid assignment target.")
          expr
    else expr

  private def or =
    var expr = and
    while matches(Or) do
      val operator = previous
      val right    = and
      expr = e.Logical(expr, operator, right)
    expr

  private def and =
    var expr = equality
    while matches(And) do
      val operator = previous
      val right    = equality
      expr = e.Logical(expr, operator, right)
    expr

  private def equality =
    var expr = comparison
    while matches(BangEqual, EqualEqual) do
      val operator = previous
      val right    = comparison
      expr = e.Binary(expr, operator, right)
    expr

  private def comparison =
    var expr = term
    while matches(Greater, GreaterEqual, Less, LessEqual) do
      val operator = previous
      val right    = term
      expr = e.Binary(expr, operator, right)
    expr

  private def term =
    var expr = factor
    while matches(Minus, Plus) do
      val operator = previous
      val right    = factor
      expr = e.Binary(expr, operator, right)
    expr

  private def factor =
    var expr = unary
    while matches(Slash, Star) do
      val operator = previous
      val right    = unary
      expr = e.Binary(expr, operator, right)
    expr

  private def unary: e =
    if matches(Bang, Minus) then
      val operator = previous
      val right    = unary
      e.Unary(operator, right)
    else call

  private def call =
    var expr = primary

    var it = true
    while it do
      if matches(LeftParen) then expr = finishCall(expr)
      else if matches(Dot) then
        val name = consume(Identifier, "Expect property name after '.'.")
        expr = e.Get(expr, name)
      else it = false

    expr

  private def finishCall(callee: Expression) =
    val arguments = ListBuffer[Expression]()
    if !check(RightParen) then
      while
        if arguments.length >= 255 then error(peek, "Can't have more than 255 arguments.")
        arguments += expression
        matches(Comma)
      do ()
    val paren = consume(RightParen, "Expect ')' after arguments.")
    e.Call(callee, paren, arguments.toList)

  private def primary =
    if matches(False) then e.Literal(false)
    else if matches(True) then e.Literal(true)
    else if matches(Nil) then e.Literal(null)
    else if matches(Number, String) then e.Literal(previous.literal)
    else if matches(Super) then
      val keyword = previous
      consume(Dot, "Expect '.' after 'super'.")
      val method = consume(Identifier, "Expect superclass method name.")
      e.Super(keyword, method)
    else if matches(This) then e.This(previous)
    else if matches(Identifier) then e.Variable(previous)
    else if matches(LeftParen) then
      val expr = expression
      consume(RightParen, "Expect ')' after expression.")
      e.Grouping(expr)
    else throw error(peek, "Expect expression.")

  //////////////////////////////////////////////////////////////////////////////

  private def classDeclaration =
    val name = consume(Identifier, "Expect class name.")
    val superclass: e.Variable = if matches(Less) then
      consume(Identifier, "Expect superclass name.")
      e.Variable(previous)
    else null

    consume(LeftBrace, "Expect '{' before class body.")

    val methods = ListBuffer[s.Function]()
    while !check(RightBrace) && !atEnd do methods += function("method")

    consume(RightBrace, "Expect '}' after class body.")

    s.Class(name, superclass, methods.toList)

  private def function(kind: String): s.Function =
    val name = consume(Identifier, s"Expect $kind name.")
    consume(LeftParen, s"Expect '(' after $kind name.")

    val parameters = ListBuffer[Token]()
    if !check(RightParen) then
      while
        if parameters.length >= 255 then error(peek, "Can't have more than 255 parameters.")
        parameters += consume(Identifier, "Expect parameter name.")
        matches(Comma)
      do ()

    consume(RightParen, "Expect ')' after parameters.")
    consume(LeftBrace, s"Expect '{' before $kind body.")
    val body = block
    s.Function(name, parameters.toList, body)

  private def varDeclaration =
    val name        = consume(Identifier, "Expect variable name.")
    val initializer = if matches(Equal) then expression else null
    consume(Semicolon, "Expect ';' after variable declaration.")
    s.Var(name, initializer)

  private def forStatement =
    consume(LeftParen, "Expect '(' after 'for'.")

    val initializer =
      if matches(Semicolon)
      then null
      else if matches(Var) then varDeclaration
      else expressionStatement

    var condition = if !check(Semicolon) then expression else null
    consume(Semicolon, "Expect ';' after loop condition.")

    val increment = if !check(RightParen) then expression else null
    consume(RightParen, "Expect ')' after for clauses.")

    var body = statement

    // Desugaring of for loop into while loop
    if increment != null then body = s.Block(Array(body, s.ExpressionStatement(increment)))
    if condition == null then condition = e.Literal(true)

    body = s.While(condition, body)

    if initializer != null then body = s.Block(Array(initializer, body))

    body

  private def ifStatement =
    consume(LeftParen, "Expect '(' after 'if'.")
    val condition = expression
    consume(RightParen, "Expect ')' after if condition.")

    val thenBranch = statement
    val elseBranch = if matches(Else) then statement else null
    s.If(condition, thenBranch, elseBranch)

  private def printStatement =
    val value = expression
    consume(Semicolon, "Expect ';' after value.")
    s.Print(value)

  private def returnStatement =
    val keyword = previous
    val value   = if !check(Semicolon) then expression else null
    consume(Semicolon, "Expect ';' after return value.")
    s.Return(keyword, value)

  private def whileStatement =
    consume(LeftParen, "Expect '(' after 'while'.")
    val condition = expression
    consume(RightParen, "Expect ')' after condition.")
    val body = statement
    s.While(condition, body)

  private def block =
    val statements = ListBuffer[Statement]()
    while !check(RightBrace) && !atEnd do statements += declaration
    consume(RightBrace, "Expect '}' after block.")
    statements.toList
