package lambda

import exception.LambdaException

/**
 * λ
 *
 * // @TODO add lambda exceptions.
 */
trait LambdaExpression {

  def isLambdaExpression: Boolean = true

  def isLambdaVariable = false

  def isEmpty: Boolean

  def reduce(arg: String): LambdaExpression = reduce(new Argument(arg), "")

  def reduce(arg: Argument, newVariable: String): LambdaExpression

  def :+(that: LambdaExpression): LambdaExpression

  def toList: List[LambdaExpression]

  def lookAhead(current: LambdaExpression, expressions: List[LambdaExpression]): String = {
    if (expressions.nonEmpty) {
      current match {
        case arg: Argument => expressions.head match {
          case arg: Argument => " "
          case _: BoundVariable | _: NonEmptyLambdaExpression => " "
        }
        case bv: BoundVariable => expressions.head match {
          case arg: Argument => ""
          case _: BoundVariable | _: NonEmptyLambdaExpression => " "
        }
        case exp: NonEmptyLambdaExpression => expressions.head match {
          case arg: Argument => ""
          case _: BoundVariable | _: NonEmptyLambdaExpression => " "
        }
      }
    }
    else ""
  }

}

trait LambdaVariable extends LambdaExpression {

  override def isLambdaVariable = true

  val literal: String

}

class NonEmptyLambdaExpression(val expressions: List[LambdaExpression]) extends LambdaExpression {

  def this(lambda: LambdaExpression) = this(List(lambda))

  override def isEmpty = false

  override def toList: List[LambdaExpression] = expressions

  // @TODO flatten
  implicit def flatten(): LambdaExpression =
    new NonEmptyLambdaExpression(this.flatExpressions())

  implicit def flatExpressions(): List[LambdaExpression] =
    this.expressions.foldLeft(List(): List[LambdaExpression])((acc, curr) => curr match {
      case ex: NonEmptyLambdaExpression => acc ++ ex.flatExpressions()
      case arg: Argument => acc.:+(arg)
      case bv: BoundVariable => acc.:+(bv)
    })

  /**
   *
   * Allow LambdaExpression merging.
   *
   * @return
   */
  // @TODO
  //def +(that: NonEmptyLambdaExpression) = new NonEmptyLambdaExpression(that.expressions.++:(this.expressions))

  /**
   * @param that
   * @return
   */
  // @TODO
  def +(that: List[NonEmptyLambdaExpression]) = this.expressions.:+(that)

  override def :+(that: LambdaExpression) = new NonEmptyLambdaExpression(expressions.:+(that))

  def ++(that: Argument) =
    if (expressions.isEmpty) new NonEmptyLambdaExpression(that)
    else new NonEmptyLambdaExpression(expressions.+:(that))

  override def toString = expressions.map(_.toString).reduceLeft {
    _ + " " + _
  }

  def length: Int = expressions.length

  override def reduce(arg: Argument, newVariable: String): LambdaExpression =
    new NonEmptyLambdaExpression(expressions.map(_.reduce(arg, newVariable)))

}

object EmptyLambdaExpression extends LambdaExpression {

  def isEmpty = true

  def empty: LambdaExpression = this

  override def :+(lambda: LambdaExpression): LambdaExpression = new NonEmptyLambdaExpression(List(lambda))

  override def reduce(arg: Argument, newVariable: String): LambdaExpression = EmptyLambdaExpression

  override def toString = throw new LambdaException("Empty expression has no toString method.")

  override def toList: List[LambdaExpression] = throw new LambdaException("Empty expression has no toList method.")

}


class BoundVariable(val literal: String, val expression: NonEmptyLambdaExpression) extends LambdaVariable {

  def this(literal: String, lambda: LambdaExpression) = this(literal, new NonEmptyLambdaExpression(List(lambda)))

  override def isEmpty = false

  override def toString = "λ" + literal.toString + "." + argumentsToString

  def argumentsToString =
    if (expression.length == 0) ""
    else if (expression.length > 1) "(" + expression.toString + ")"
    else expression.toString


  // @TODO
  override def :+(that: LambdaExpression): LambdaExpression = ???

  override def toList: List[LambdaExpression] = expression.expressions

  override def reduce(arg: Argument, newVariable: String): LambdaExpression =
    if (newVariable.isEmpty)
      new NonEmptyLambdaExpression(expression.expressions.map(_.reduce(arg, literal)))
    else
      new BoundVariable(literal, new NonEmptyLambdaExpression(expression.expressions.map(_.reduce(arg, newVariable))))
}


class Argument(val literal: String) extends LambdaVariable {
  override def isEmpty: Boolean = false

  override def toString = literal

  override def reduce(arg: Argument, newVariable: String): LambdaExpression =
    if (literal == newVariable) new Argument(arg.literal) else this

  // @TODO
  override def :+(that: LambdaExpression): LambdaExpression = ???

  override def toList: List[LambdaExpression] = ???

}