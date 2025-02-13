package org.willena.slox

import scala.annotation.tailrec

class LoxClass(val name: String, superclass: LoxClass, methods: Map[String, LoxFunction]) extends LoxCallable:

  @tailrec
  final def findMethod(name: String): Option[LoxFunction] =
    methods.get(name) match
      case method @ Some(_) => method
      case None =>
        Option(superclass) match
          case None         => None
          case Some(sclass) => sclass.findMethod(name)

  override def arity: Int = findMethod("init") match
    case Some(method) => method.arity
    case None         => 0

  override def call(interpreter: Interpreter, arguments: Seq[Any]): Any =
    val instance = LoxInstance(this)
    findMethod("init").foreach(_.bind(instance).call(interpreter, arguments))
    instance

  override def toString: String = name
