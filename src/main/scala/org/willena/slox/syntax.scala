package org.willena.slox

import org.willena.slox.Expression.Variable

enum Expression:
  case Assign(name: Token, value: Expression)
  case Binary(left: Expression, operator: Token, right: Expression)
  case Call(callee: Expression, paren: Token, arguments: Seq[Expression])
  case Get(obj: Expression, name: Token)
  case Grouping(expr: Expression)
  case Literal(value: LiteralValue)
  case Logical(left: Expression, operator: Token, right: Expression)
  case Set(obj: Expression, name: Token, value: Expression)
  case Super(keyword: Token, method: Token)
  case This(keyword: Token)
  case Unary(operator: Token, right: Expression)
  case Variable(name: Token)

enum Statement:
  case Block(statements: Seq[Statement])
  case Class(
      name: Token,
      superclass: Variable, // TODO superclass is null when there's no superclass
      methods: Seq[Function],
  ) 
  case ExpressionStatement(expression: Expression)
  case Function(name: Token, params: Seq[Token], body: Seq[Statement])
  case If(
      condition: Expression,
      thenBranch: Statement,
      elseBranch: Statement, // TODO elseBranch is null when there's no else condition
  )
  case Print(expression: Expression)
  case Return(keyword: Token, value: Expression) // TODO value is null when there's no return value
  case Var(name: Token, initializer: Expression) // TODO initializer is null when the variable is just declared
  case While(condition: Expression, body: Statement)
