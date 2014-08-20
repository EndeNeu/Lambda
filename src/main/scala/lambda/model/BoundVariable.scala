package lambda.model

class BoundVariable(val literal: String, val lambdas: List[LambdaExpression]) extends LambdaVariable {

  override def isEmpty = false

  override def toString = "λ" + literal.toString + "." + argumentsToString

  def argumentsToString =
    if (lambdas.length == 0) ""
    else if (lambdas.length > 1) "(" + stringifyLambdas() + ")"
    else stringifyLambdas()

  def stringifyLambdas() =
    lambdas.map(_.toString).reduce {
      _ + " " + _
    }

  override def toList: List[LambdaExpression] =
    lambdas

  /**
   * Given an argument, reduce a lambda expression.
   *
   * @param arg
   * @param newVariable
   * @return
   */
  override def betaReduce(arg: LambdaExpression, newVariable: String): LambdaExpression = {
    // if we don't have a bound variable yet, reduce this bound variable arguments wrapping everything in a EX
    if (newVariable.isEmpty) new NonEmptyLambdaExpression(lambdas.map(_.betaReduce(arg, literal)))
    // if instead we already have a variable to reduce but this is a bound variable
    // with the same literal, hence this is a free variable, do nothing.
    else if (newVariable == literal) this
    // otherwise we do have a variable to reduce, reduce this bound variable
    else new BoundVariable(literal, lambdas.map(_.betaReduce(arg, newVariable)))
  }

  /**
   * Allow chaining using a bound variable.
   */
  def :+(that: BoundVariable): LambdaExpression =
    new BoundVariable(literal, lambdas ++ that.lambdas)

  /**
   * Allow chaining using a bound variable.
   */
  def +:(that: BoundVariable): LambdaExpression =
    new BoundVariable(literal, lambdas ++ that.lambdas)

  override def :+(that: NonEmptyLambdaExpression): LambdaExpression =
    new BoundVariable(literal, lambdas ++ that.lambdas)

  override def +:(that: NonEmptyLambdaExpression): LambdaExpression =
    new BoundVariable(literal, lambdas ++ that.lambdas)

  override def getStructure(): String =
    "BV(" + lambdas.foldLeft("") { _ + _.getStructure() } + ")"
}