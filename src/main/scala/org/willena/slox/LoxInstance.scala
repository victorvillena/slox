package org.willena.slox

import scala.collection.mutable

class LoxInstance(klass: LoxClass):
 
  private val fields = mutable.Map[String, Any]()

  def get(name: Token) =
    fields
      .get(name.lexeme)
      .orElse(Option(klass.findMethod(name.lexeme)).map(_.bind(this)))
      .getOrElse(throw RuntimeError(name, s"Undefined property '${name.lexeme}'.'"))

  def set(name: Token, value: Any) = fields.update(name.lexeme, value)

  override def toString: String = s"${klass.name} instance"
