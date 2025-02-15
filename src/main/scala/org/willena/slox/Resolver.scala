package org.willena.slox

import scala.collection.mutable

import org.willena.slox.ClassType.{NoClass, Subclass}
import org.willena.slox.FunctionType.{Initializer, NoFunction}

private enum FunctionType:
  case NoFunction, Function, Initializer, Method

private enum ClassType:
  case NoClass, Class, Subclass

class Resolver(val interpreter: Interpreter):

  private val scopes: mutable.Stack[mutable.Map[String, Boolean]] = mutable.Stack.empty

  private var currentFunction = FunctionType.NoFunction
  private var currentClass    = ClassType.NoClass

  // Helpers ///////////////////////////////////////////////////////////////////

  private def scope(condition: => Boolean = true, action: => Unit = ())(block: => Unit) =
    if condition then
      scopes.push(mutable.HashMap.empty)
      action
      block
      scopes.pop()
    else block

  private def declare(name: Token): Unit =
    if scopes.nonEmpty then
      val scope = scopes.top
      if scope.contains(name.lexeme) then Lox.error(name, "Already a variable with this name in this scope.")
      else scope.put(name.lexeme, false)

  private def define(name: Token): Unit =
    if scopes.nonEmpty then scopes.top.put(name.lexeme, true)

  //////////////////////////////////////////////////////////////////////////////

  def resolve(statements: Seq[Statement]): Unit = statements.foreach(resolve)

  def resolve(statement: Statement): Unit =
    statement match
      case Statement.Block(statements) => scope()(resolve(statements))

      case Statement.Class(name, superclass, methods) =>
        import ClassType.*
        import FunctionType.*

        val enclosingClass = currentClass
        currentClass = Class

        declare(name)
        define(name)

        if superclass != null && name.lexeme == superclass.name.lexeme then
          Lox.error(superclass.name, "A class can't inherit from itself.")

        if superclass != null then
          currentClass = Subclass
          resolve(superclass)

        scope(superclass != null, scopes.top.put("super", true)):
          scope():
            scopes.top.put("this", true)

            methods.foreach: method =>
              val declaration = if method.name.lexeme == "init" then Initializer else Method
              resolveFunction(method, declaration)

        currentClass = enclosingClass

      case Statement.ExpressionStatement(expression) => resolve(expression)

      case fun @ Statement.Function(name, _, _) =>
        declare(name)
        define(name)
        resolveFunction(fun, FunctionType.Function)

      case Statement.If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        if elseBranch != null then resolve(elseBranch)

      case Statement.Print(expression) => resolve(expression)

      case Statement.Return(keyword, value) =>
        if currentFunction == NoFunction then Lox.error(keyword, "Can't return from top-level code.")
        if value != null then
          if currentFunction == Initializer then Lox.error(keyword, "Can't return a value from an initializer.")
          resolve(value)

      case Statement.Var(name, initializer) =>
        declare(name)
        if initializer != null then resolve(initializer)
        define(name)

      case Statement.While(condition, body) =>
        resolve(condition)
        resolve(body)

  def resolve(expression: Expression): Unit =
    expression match
      case Expression.Assign(name, value) =>
        resolve(value)
        resolveLocal(expression, name)
      case Expression.Binary(left, _, right) =>
        resolve(right)
        resolve(left)
      case Expression.Call(callee, _, arguments) =>
        resolve(callee)
        arguments.foreach(resolve)
      case Expression.Get(obj, name) =>
        resolve(obj)
      case Expression.Grouping(expr) =>
        resolve(expr)
      case Expression.Literal(value) => ()
      case Expression.Logical(left, _, right) =>
        resolve(left)
        resolve(right)
      case Expression.Set(obj, _, value) =>
        resolve(value)
        resolve(obj)
      case Expression.Super(keyword, method) =>
        if currentClass == NoClass then Lox.error(keyword, "Can't use 'super' outside of a class.")
        else if currentClass != Subclass then Lox.error(keyword, "Can't use 'super' in a class with no superclass.")
        resolveLocal(expression, keyword)
      case Expression.This(keyword) =>
        if currentClass == NoClass then Lox.error(keyword, "Can't use 'this' outside of a class.")
        resolveLocal(expression, keyword)
      case Expression.Unary(operator, right) =>
        resolve(right)
      case Expression.Variable(name) =>
        if scopes.nonEmpty && !scopes.top.getOrElse(name.lexeme, true) then
          Lox.error(name, "Can't read local variable in its own initializer.")
        resolveLocal(expression, name)

  private def resolveLocal(expression: Expression, name: Token) =
    val depth = scopes.indexWhere(_.contains(name.lexeme))
    if depth >= 0 then interpreter.resolve(expression, depth)

  private def resolveFunction(fun: Statement.Function, functionType: FunctionType) =
    val enclosingFunction = currentFunction
    currentFunction = functionType

    scope():
      fun.params.foreach: param =>
        declare(param)
        define(param)
      resolve(fun.body)

    currentFunction = enclosingFunction
