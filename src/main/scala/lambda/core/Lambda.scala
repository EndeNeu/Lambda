package lambda.core

import lambda.LambdaBuilder.{AR, BV, EX}
import lambda.model.{BoundVariable, NonEmptyLambdaExpression}

/**
 * Contains some fuoundamental lambda functions.
 */
object Lambda {

  def simpleLambda(variable: String): BoundVariable = BV(variable, AR(variable))

  /**
   * λx.x
   */
  def identity(x: String): NonEmptyLambdaExpression = EX(simpleLambda(x))

  /**
   * λx.(x x)
   */
  def selfApply(x: String): NonEmptyLambdaExpression =
    EX(
      BV(x,
        AR(x),
        AR(x)
      )
    )

  /**
   * λx.λy.(x y)
   */
  def lApply(x: String, y: String) =
    EX(
      BV(
        x,
        BV(
          y,
          AR(x),
          AR(y)
        )
      )
    )

}