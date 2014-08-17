package lambda.model

import lambda.exception.LambdaException

object EmptyLambdaExpression extends LambdaExpression {

  def isEmpty = true

  def empty: LambdaExpression = this

  override def betaReduce(arg: LambdaExpression, newVariable: String): LambdaExpression = EmptyLambdaExpression

  override def toString =
    throw new LambdaException("Empty expression has no toString method.")

  override def toList: List[LambdaExpression] =
    throw new LambdaException("Empty expression has no toList method.")

  /**
   * Chaining with an empty expression always returns that.
   */
  override def :+(that: NonEmptyLambdaExpression): LambdaExpression = that

  override def +:(that: NonEmptyLambdaExpression): LambdaExpression = that

}
