package org.willena.slox

import scala.annotation.tailrec
import scala.collection.mutable

class Environment(val enclosing: Environment = null):

  private val values: mutable.Map[String, Any] = mutable.HashMap.empty

  def define(name: String, value: Any) = values.put(name, value)

  def getAt(distance: Int, name: String) = ancestor(distance).values(name)

  def assignAt(distance: Int, name: Token, value: Any) = ancestor(distance).values.put(name.lexeme, value)

  @tailrec
  final def get(name: Token): Any =
    values.get(name.lexeme) match
      case Some(value) => value
      case None =>
        if enclosing == null then throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        else enclosing.get(name)

  @tailrec
  final def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then values.put(name.lexeme, value)
    else if enclosing != null then
      // Assign to enclosing env's variable if that where the variable was defined. A 'var'
      // definition would create a variable in the current environment, but plain assignment
      // uses the outer env's variables. This is how global vars work.
      enclosing.assign(name, value)
    else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")

  @tailrec
  private def ancestor(distance: Int): Environment =
    if distance == 0 then this else enclosing.ancestor(distance - 1) // Todo what if null?
