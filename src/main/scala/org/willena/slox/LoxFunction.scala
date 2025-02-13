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
    Try(interpreter.executeBlock(declaration.body, environment)).fold(
      {
        case returnValue: Return =>
          if isInit then
            // Only value-less returns in initializers reach this. Returns with values in initializers are caught by the
            // resolver.
            closure.getAt(0, "this")
          else returnValue.value
        case e => throw e
      },
      { _ =>
        // Make init methods always return 'this' when called
        if isInit then closure.getAt(0, "this")
        else null
      },
    )

  override def arity: Int = declaration.params.length

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
