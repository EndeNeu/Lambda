package lambda

object LambdaBuilder {

  /**
   *
   * EX(boundvar, arg)
   *
   */
  object EX {
    def apply(expressions: LambdaExpression*) =
      new NonEmptyLambdaExpression(expressions.toList)


    def apply(tuple: (LambdaExpression, LambdaExpression)) =
      new NonEmptyLambdaExpression(List(tuple._1, tuple._2))

  }

  object BV {

    def apply(x: String, expression: NonEmptyLambdaExpression) =
      new BoundVariable(x, expression.toList)

    def apply(x: String, expression: LambdaExpression*) =
      new BoundVariable(x, expression.toList)
  }

  object AR {
    def apply(x: String) = new Argument(x)
  }

}
