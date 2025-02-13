package org.willena.slox

import scala.util.Try

class LoxFunction(declaration: Statement.Function, closure: Environment, isInit: Boolean) extends LoxCallable:

  def bind(instance: LoxInstance) =
    val environment = Environment(closure)
    environment.define("this", instance)
    LoxFunction(declaration, environment, isInit)

  override def call(interpreter: Interpreter, arguments: Seq[Any]): Any =
    val environment = Environment(closure)
    declaration.params
      .zip(arguments)
      .map: (param, arg) =>
        environment.define(param.lexeme, arg)

    // TODO this might be much clearer if the return used a concrete type instead of a null
    var returnValue: Any = null
    try
      interpreter.executeBlock(declaration.body, environment)
      if isInit then returnValue = closure.getAt(0, "this")
    catch
      case value: Return =>
        returnValue = if isInit then
          closure.getAt(0, "this") // value-less return in initializer (if there's a value it's an error)
        else value.value

    returnValue

  override def arity: Int = declaration.params.length

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
