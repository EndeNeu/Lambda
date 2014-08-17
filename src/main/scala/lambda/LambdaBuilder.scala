package lambda

import model.{Argument, BoundVariable, NonEmptyLambdaExpression, LambdaExpression}

/**
 * Provides utility methods to build lambda definitons.
 */
object LambdaBuilder {

  object EX {
    def apply(expressions: LambdaExpression*) =
      new NonEmptyLambdaExpression(expressions.toList)

    def apply(tuple: (LambdaExpression, LambdaExpression)) =
      new NonEmptyLambdaExpression(List(tuple._1, tuple._2))

  }

  object BV {
    def apply(x: String, expression: NonEmptyLambdaExpression) =
      new BoundVariable(x, expression.lambdas)

    def apply(x: String, expression: LambdaExpression*) =
      new BoundVariable(x, expression.toList)
  }

  object AR {
    def apply(x: String) = new Argument(x)
  }

}
