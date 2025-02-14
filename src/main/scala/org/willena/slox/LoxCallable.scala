package org.willena.slox

trait LoxCallable:
  def arity: Int
  def call(interpreter: Interpreter, arguments: Seq[Any]): Any
