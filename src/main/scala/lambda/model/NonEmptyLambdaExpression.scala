package lambda.model

class NonEmptyLambdaExpression(val lambdas: List[LambdaExpression]) extends LambdaExpression {

  def this(lambda: LambdaExpression) = this(List(lambda))

  override def isEmpty = false

  override def toList: List[LambdaExpression] = lambdas

  /**
   * Flattens a NonEmptyLambdaExpression which contains other NonEmptyLambdaExpressions
   */
  implicit def flatten(): LambdaExpression =
    new NonEmptyLambdaExpression(this.flatExpressions())

  /**
   * Flattens a list of LambdaExpressions.
   */
  implicit def flatExpressions(): List[LambdaExpression] =
    this.lambdas.foldLeft(List(): List[LambdaExpression])((acc, curr) => curr match {
      case ex: NonEmptyLambdaExpression => acc ++ ex.flatExpressions()
      case arg: Argument => acc.:+(arg)
      case bv: BoundVariable => acc.:+(bv)
    })

  /**
   * Allow LambdaExpression merging (append).
   */
  override def :+(that: NonEmptyLambdaExpression) =
    new NonEmptyLambdaExpression(this.lambdas ++ that.lambdas)

  /**
   * Allow LambdaExpression merging (prepend).
   */
  override def +:(that: NonEmptyLambdaExpression) =
    new NonEmptyLambdaExpression(that.lambdas ++ this.lambdas)

  def ++(that: Argument) =
    if (lambdas.isEmpty) new NonEmptyLambdaExpression(that)
    else new NonEmptyLambdaExpression(lambdas.+:(that))

  override def toString = lambdas.map(_.toString).reduceLeft {
    _ + " " + _
  }

  def length: Int = lambdas.length

  override def betaReduce(arg: LambdaExpression, newVariable: String): LambdaExpression =
    new NonEmptyLambdaExpression(lambdas.map(_.betaReduce(arg, newVariable)))

  override def betaReduce(): LambdaExpression = {
    EmptyLambdaExpression
  }

}
