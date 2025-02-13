package org.willena.slox

import scala.annotation.tailrec
import scala.collection.mutable

class Environment(val enclosing: Environment = null):

  private val values = mutable.Map[String, Any]()

  def define(name: String, value: Any) = values.put(name, value)

  def getAt(distance: Int, name: String) = ancestor(distance).values(name)

  // TODO isn't this a generalized 'define'? why a separate 'define'?
  def assignAt(distance: Int, name: Token, value: Any) = ancestor(distance).values.put(name.lexeme, value)

  def get(name: Token): Any =
    values
      .get(name.lexeme)
      .orElse(Option(enclosing).map(_.get(name)))
      .getOrElse(throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))

  @tailrec
  final def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then values.put(name.lexeme, value)
    else if enclosing != null then
      // Assign to enclosing env's variable if that where the variable was defined. A 'var'
      // definition would create a variable in the current environment, but plain assignment
      // uses the outer env's variables. This is how global vars work.
      enclosing.assign(name, value)

  @tailrec
  private def ancestor(distance: Int): Environment =
    if distance == 0 then this else enclosing.ancestor(distance - 1) // Todo what if null?
