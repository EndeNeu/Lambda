package lambda.model

class BoundVariable(val literal: String, val nonEmptyLambda: NonEmptyLambdaExpression) extends LambdaVariable {

  def this(literal: String, lambda: LambdaExpression) = this(literal, new NonEmptyLambdaExpression(List(lambda)))

  override def isEmpty = false

  override def toString = "λ" + literal.toString + "." + argumentsToString

  def argumentsToString =
    if (nonEmptyLambda.length == 0) ""
    else if (nonEmptyLambda.length > 1) "(" + nonEmptyLambda.toString + ")"
    else nonEmptyLambda.toString

  override def toList: List[LambdaExpression] = nonEmptyLambda.lambdas

  /**
   * Given an argument, reduce a lambda expression to the minimum terms.
   *
   * @param arg
   * @param newVariable
   * @return
   */
  override def reduce(arg: Argument, newVariable: String): LambdaExpression =
  // if we don't have a bound variable yet, reduce this bound variable
    if (newVariable.isEmpty)
      new NonEmptyLambdaExpression(nonEmptyLambda.lambdas.map(_.reduce(arg, literal)))
    // if instead this is a free variable do nothing
    else if (newVariable == literal)
      this
    // otherwise recude this bound variable
    else
      new BoundVariable(literal, new NonEmptyLambdaExpression(nonEmptyLambda.lambdas.map(_.reduce(arg, newVariable))))

  /**
   * Allow chaining using a bound variable.
   */
  def :+(that: BoundVariable): LambdaExpression =
    this.:+(that.nonEmptyLambda)

  /**
   * Allow chaining using a bound variable.
   */
  def +:(that: BoundVariable): LambdaExpression =
    this.+:(that.nonEmptyLambda)

  override def :+(that: NonEmptyLambdaExpression): LambdaExpression =
    new BoundVariable(literal, nonEmptyLambda.:+(that))

  override def +:(that: NonEmptyLambdaExpression): LambdaExpression =
    new BoundVariable(literal, nonEmptyLambda.+:(that))

}
