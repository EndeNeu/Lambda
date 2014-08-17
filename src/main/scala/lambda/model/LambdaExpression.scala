package lambda.model

import lambda.exception.LambdaException

/**
 * λ
 *
 * Defines a common trait for lambda components.
 */
trait LambdaExpression {

  def isLambdaExpression: Boolean = true

  def isLambdaVariable = false

  def isEmpty: Boolean

  def betaReduce(arg: String): LambdaExpression = betaReduce(new Argument(arg), "")

  def betaReduce(arg: LambdaExpression): LambdaExpression = betaReduce(arg, "")

  def betaReduce(arg: LambdaExpression, newVariable: String): LambdaExpression

  def betaReduce(reduceAll: Boolean = false): LambdaExpression =
    throw new LambdaException("Parameterless beta reduce is not supported on " + this.getClass)

  def :+(that: NonEmptyLambdaExpression): LambdaExpression

  def +:(that: NonEmptyLambdaExpression): LambdaExpression

  def toList: List[LambdaExpression]

  /**
   * Helper method that look ahead in the lambda to fix some spacing issues
   * when outputting the expression to string.
   *
   * @param current
   * @param expressions
   * @return
   */
  def lookAhead(current: LambdaExpression, expressions: List[LambdaExpression]): String = {
    if (expressions.nonEmpty) {
      current match {
        case arg: Argument => expressions.head match {
          case arg: Argument => " "
          case _ => " "
        }
        case variable@(_: BoundVariable | _: NonEmptyLambdaExpression) => expressions.head match {
          case arg: Argument => ""
          case _ => " "
        }
      }
    }
    else ""
  }

}

/**
 * Common trait to lambda variables
 */
trait LambdaVariable extends LambdaExpression {

  override def isLambdaVariable = true

  val literal: String

}
