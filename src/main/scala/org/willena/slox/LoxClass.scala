package org.willena.slox

class LoxClass(val name: String, superclass: LoxClass, methods: Map[String, LoxFunction]) extends LoxCallable:

  // TODO maybe return Option?
  def findMethod(name: String): LoxFunction =
    methods
      .get(name)
      .orElse(Option(superclass).map(_.findMethod(name)))
      .orNull

  override def arity: Int = Option(findMethod("init")).map(_.arity).getOrElse(0)

  override def call(interpreter: Interpreter, arguments: Seq[Any]): Any =
    val instance = LoxInstance(this)
    Option(findMethod("init")).foreach(_.bind(instance).call(interpreter, arguments))
    instance
    

  override def toString: String = name
