package org.willena.slox

import scala.collection.mutable

class Interpreter:

  val globals             = Environment()
  private var environment = globals
  private val locals      = mutable.HashMap[Expression, Int]()

  globals.define(
    "clock",
    new LoxCallable:
      override def arity: Int                                          = 0
      override def toString: String                                    = "<native fn>"
      override def call(interpreter: Interpreter, arguments: Seq[Any]) = System.currentTimeMillis() / 1000.0,
  )

  def resolve(expression: Expression, depth: Int) = locals.update(expression, depth)

  // Helpers ///////////////////////////////////////////////////////////////////

  private def stringify(obj: Any) =
    obj match
      case null      => "nil"
      case d: Double => d.toString.stripSuffix(".0") // GAIN Scala stdlib goodies
      case _         => obj.toString

  private def isEqual(a: Any, b: Any) =
    (a, b) match
      case (null, null) => true
      case (null, _)    => false
      case (_, _)       => a == b

  private def isTruthy(obj: Any) =
    obj match
      case null       => false
      case b: Boolean => b
      case _          => true

  private def checkNumberOperand(operator: Token, a: Any) =
    a match
      case d: Double => d
      case _         => throw RuntimeError(operator, "Operand must be a number.")

  private def checkNumberOperands(operator: Token, a: Any, b: Any) =
    (a, b) match
      case (d1: Double, d2: Double) => (d1, d2)
      case _                        => throw RuntimeError(operator, "Operands must be numbers.")

  //////////////////////////////////////////////////////////////////////////////

  def interpret(statements: Seq[Statement]) =
    try statements.foreach(execute)
    catch case error: RuntimeError => Lox.runtimeError(error)

  // GAIN All that complex visitor pattern setup turns into simple pattern matching

  private def evaluate(expression: Expression): Any =
    expression match
      case Expression.Assign(name, value) =>
        val evaluated = evaluate(value)
        locals.get(expression) match
          case Some(distance) => environment.assignAt(distance, name, evaluated)
          case None           => globals.assign(name, evaluated)
        evaluated

      case Expression.Binary(left, operator, right) =>
        import TokenType.*
        val l = evaluate(left)
        val r = evaluate(right)
        operator.tpe match
          case BangEqual  => !isEqual(l, r)
          case EqualEqual => isEqual(l, r)
          case Greater =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 > d2
          case GreaterEqual =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 >= d2
          case Less =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 < d2
          case LessEqual =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 <= d2
          case Minus =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 - d2
          case Slash =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 / d2
          case Star =>
            val (d1, d2) = checkNumberOperands(operator, l, r)
            d1 * d2
          case Plus =>
            (l, r) match
              case (d1: Double, d2: Double) => d1 + d2 // sum of numbers
              case (d1: String, d2: String) => d1 + d2 // string concatenation
              case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
          case _ => ??? // unreachable

      case Expression.Call(callee, paren, arguments) =>
        val evaluated     = evaluate(callee)
        val evaluatedArgs = arguments.map(evaluate)

        evaluated match
          case function: LoxCallable =>
            if evaluatedArgs.length == function.arity then function.call(this, evaluatedArgs)
            else throw RuntimeError(paren, s"Expected ${function.arity} arguments but got ${evaluatedArgs.length}.")
          case _ =>
            throw RuntimeError(paren, "Can only call functions and classes.")

      case Expression.Get(obj, name) =>
        evaluate(obj) match
          case instance: LoxInstance => instance.get(name)
          case _                     => throw RuntimeError(name, "Only instances have properties.")

      case Expression.Grouping(expr) => evaluate(expr)

      case Expression.Literal(value) => value

      case Expression.Logical(left, operator, right) =>
        import TokenType.*
        val l = evaluate(left)

        operator.tpe match
          case Or if isTruthy(l)   => l // Short-circuited OR if left operand is true
          case And if !isTruthy(l) => l // Short-circuited AND if left operand is false
          case _                   => evaluate(right)

      case Expression.Set(obj, name, value) =>
        evaluate(obj) match
          case instance: LoxInstance =>
            val setValue = evaluate(value)
            instance.set(name, setValue)
            value
          case _ =>
            throw RuntimeError(name, "Only instances have fields.")

      case Expression.Super(keyword, method) =>
        val distance   = locals(expression)
        val superclass = environment.getAt(distance, "super").asInstanceOf[LoxClass]
        val obj        = environment.getAt(distance - 1, "this").asInstanceOf[LoxInstance]

        val superMethod = superclass.findMethod(method.lexeme)
        if superMethod == null then throw RuntimeError(method, s"Undefined property '${method.lexeme}'.")
        else superMethod.bind(obj)

      case Expression.This(keyword) => lookupVariable(keyword, expression)

      case Expression.Unary(operator, right) =>
        import TokenType.*
        val value = evaluate(right)
        operator.tpe match
          case Bang => !isTruthy(value)
          case Minus =>
            val n = checkNumberOperand(operator, value)
            -n
          case _ => ??? // unreachable

      case Expression.Variable(name) => lookupVariable(name, expression)

  private def execute(statement: Statement): Unit =
    statement match
      case Statement.Block(statements) =>
        executeBlock(statements, Environment(environment))

      case Statement.Class(name, superclassVariable, methods) =>
        val superclass =
          if superclassVariable != null then
            evaluate(superclassVariable) match
              case loxClass: LoxClass => loxClass
              case _                  => throw RuntimeError(superclassVariable.name, "Superclass must be a class.")
          else null

        environment.define(name.lexeme, null)

        if superclassVariable != null then
          environment = Environment(environment)
          environment.define("super", superclass)

        val methodsMap = methods
          .map: method =>
            (method.name.lexeme, LoxFunction(method, environment, method.name.lexeme == "init"))
          .toMap

        val loxClass = LoxClass(name.lexeme, superclass, methodsMap)

        if superclassVariable != null then environment = environment.enclosing

        environment.assign(name, loxClass)

      case Statement.ExpressionStatement(expression) =>
        evaluate(expression)

      case fun @ Statement.Function(name, _, _) =>
        val function = LoxFunction(fun, environment, false)
        environment.define(name.lexeme, function)

      case Statement.If(condition, thenBranch, elseBranch) =>
        if isTruthy(evaluate(condition)) then execute(thenBranch)
        else if elseBranch != null then execute(elseBranch)

      case Statement.Print(expression) =>
        println(stringify(evaluate(expression)))

      case Statement.Return(_, value) =>
        throw Return(if value != null then evaluate(value) else null)

      case Statement.Var(name, initializer) =>
        val value = if initializer != null then evaluate(initializer) else null
        environment.define(name.lexeme, value)

      case Statement.While(condition, body) =>
        while isTruthy(evaluate(condition)) do execute(body)

  def executeBlock(statements: Seq[Statement], env: Environment) =
    val previous = environment

    try
      environment = env
      statements.foreach(execute)
    finally environment = previous

  private def lookupVariable(name: Token, expression: Expression) =
    locals.get(expression) match
      case Some(distance) => environment.getAt(distance, name.lexeme)
      case None           => globals.get(name)
