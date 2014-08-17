package lambda.model


class NonEmptyLambdaExpression(val lambdas: List[LambdaExpression]) extends LambdaExpression {

  def this(lambda: LambdaExpression) = this(List(lambda))

  override def isEmpty = false

  override def toList: List[LambdaExpression] = lambdas

  /**
   * Flattens a NonEmptyLambdaExpression which contains other NonEmptyLambdaExpressions
   */
  def flatten(): LambdaExpression =
    new NonEmptyLambdaExpression(this.flatExpressions())

  /**
   * Flattens a list of LambdaExpressions.
   */
  def flatExpressions(): List[LambdaExpression] =
    flatList(lambdas)

  /**
   * Helper method
   */
  def flatList(expressions: List[LambdaExpression]): List[LambdaExpression] =
    expressions.foldLeft(List(): List[LambdaExpression])((acc, curr) => curr match {
      case ex: NonEmptyLambdaExpression => acc ++ ex.flatExpressions()
      case variable@(_: Argument | _: BoundVariable) => acc.:+(variable)
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

  override def betaReduce(reduceAll: Boolean = false): LambdaExpression = {
    def iterate(expressions: List[LambdaExpression]): List[LambdaExpression] = {
      val reduced = betaReducerHelper(expressions)
      if (reduced != expressions && reduceAll) iterate(reduced)
      else reduced
    }

    new NonEmptyLambdaExpression(iterate(lambdas))
  }

  /**
  * Helper function that folds a list of lambdas
  */
  private def betaReducerHelper(expressions: List[LambdaExpression]): List[LambdaExpression] =
  // @TODO avoid wrapping bound variable reduction in a non empty expression.
  // the flat list is necessary because lambda reductions for bound variables have a NonEmptyLambdaExpression as wrapper
  // that means that a reduced bound variable is always wrapped in a NonEmpty, to avoid problems
  // calculating list values, we flatten the result extracting the lambdas from the expression.
    flatList(
      expressions.foldLeft(List[LambdaExpression]())((acc, curr) => {
        if (acc.isEmpty) acc.+:(curr)
        else acc.last match {
          case arg: Argument => acc.:+(curr)
          case bv: BoundVariable => acc.dropRight(1).:+(bv.betaReduce(curr))
          case exp: NonEmptyLambdaExpression => acc.dropRight(1).:+(exp.betaReduce())
        }
      })
    )

}
