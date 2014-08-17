package lambda.model

import lambda.exception.LambdaException

class Argument(val literal: String) extends LambdaVariable {

  override def isEmpty: Boolean = false

  override def toString = literal

  /**
   * Reduce on arguments should either yield the argument itself, if
   * the bound variable is different from the argument literal
   * or a new argument with the bound variable as literal.
   *
   * @param arg: Argument
   * @param newVariable: String
   *
   * @return Argument
   */
  override def betaReduce(arg: LambdaExpression, newVariable: String): LambdaExpression =
    if (literal == newVariable) arg else this

  override def toList: List[LambdaExpression] =
    throw new LambdaException("toList method is undefined for arguments.")

  override def :+(that: NonEmptyLambdaExpression): LambdaExpression =
    throw new LambdaException("append method is undefined for arguments.")

  override def +:(that: NonEmptyLambdaExpression): LambdaExpression =
    throw new LambdaException("prepend method is undefined for arguments.")

}
