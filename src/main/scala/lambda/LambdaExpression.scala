package lambda


/**
 * λ
 */
trait LambdaExpression {

  def isLambdaExpression: Boolean = true

  def isLambdaVariable = false

  def isEmpty: Boolean

  def reduce(arg: Argument, boundVariable: String): LambdaExpression

  def reduce(arg: String): LambdaExpression

  def :+(that: LambdaExpression): LambdaExpression

}

trait LambdaVariable extends LambdaExpression {

  override def isLambdaVariable = true

  val literal: String

}

class NonEmptyLambdaExpression(val expressions: List[LambdaExpression]) extends LambdaExpression {

  def this(lambda: LambdaExpression) = this(List(lambda))

  override def isEmpty = false

  def toList = expressions

  /**
   *
   * Allow LambdaExpression merging.
   *
   * @param that
   * @return
   */
  // @TODO
  def +(that: NonEmptyLambdaExpression) = new NonEmptyLambdaExpression(that.expressions.++:(this.expressions))

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

  /**
   *
   * @return
   */
  override def toString = expressions.foldLeft("")((acc, current) => current match {
    case bv: BoundVariable => acc + "λ" + bv.literal.toString + "." + bv.argumentsToString + " "
    case ex: NonEmptyLambdaExpression => acc + "(" + ex.toString + ")"
    case arg: Argument => acc + arg.toString + " "
  }).trim

  def length: Int = expressions.length

  /**
   *
   *
   * @return
   */
  override def reduce(arg: Argument, boundVariable: String = ""): LambdaExpression = {
    expressions.fold(EmptyLambdaExpression)((newEX, currentEX) =>
      currentEX match {
        case bv: BoundVariable =>
          newEX.:+(bv.reduce(arg, bv.literal))
        case myArg: Argument =>
          newEX.:+(myArg)
      })
  }

  override def reduce(arg: String): LambdaExpression =
    reduce(new Argument(arg))
}

object EmptyLambdaExpression extends LambdaExpression {

  def isEmpty = true

  def empty: NonEmptyLambdaExpression = new NonEmptyLambdaExpression(List())

  override def :+(lambda: LambdaExpression): LambdaExpression = new NonEmptyLambdaExpression(List(lambda))

  override def reduce(arg: Argument, boundVariable: String = ""): LambdaExpression = EmptyLambdaExpression

  override def reduce(arg: String): LambdaExpression = EmptyLambdaExpression

  override def toString = throw new Exception("Empty expression has no toString method.")

}


class BoundVariable(val literal: String, val arguments: List[LambdaExpression]) extends LambdaVariable {

  def this(literal: String, lambda: LambdaExpression) = this(literal, List(lambda))

  override def isEmpty = false

  override def toString = "λ" + literal.toString + "." + arguments.foldLeft("")((acc, curr) => {
    curr match {
      case bv: BoundVariable => bv.toString
      case arg: Argument =>
        if (argLength > 1 && !acc.contains("(")) acc + "(" + arg.literal + " "
        else if (argLength > 1) acc + arg.literal + ")"
        else acc + arg.literal + " "
    }
  }) + " "

  def argumentsToString =
    if (arguments.isEmpty) ""
    else if (argLength > 1) "(" + arguments.map(_.toString).reduce(_ + _).trim + ")"
    else arguments.map(_.toString).reduce(_ + _)

  def argLength: Int = arguments.foldLeft(0)((acc, curr) =>
    curr match {
      case ex: NonEmptyLambdaExpression => acc + ex.length
      case _: BoundVariable | _: Argument => acc + 1
    }
  )

  def toList = arguments

  /**
   *
   *
   * @param arg
   * @return
   */
  override def reduce(arg: Argument, boundVariable: String = ""): LambdaExpression = {
    arguments.foldLeft(EmptyLambdaExpression.empty)((acc, curr) => {
      curr match {
        case bv: BoundVariable =>
          if (boundVariable.isEmpty) acc.:+(bv.reduce(arg, bv.literal))
          else acc.:+(new BoundVariable(bv.literal, bv.reduce(arg, boundVariable)))
        case myArg: Argument if myArg.literal == boundVariable & !boundVariable.isEmpty =>
          acc.:+(new Argument(arg.literal))
        case myArg: Argument =>
          acc.:+(myArg)
        case _ =>
          throw new Exception("Unsupported lambda given.")
      }
    })
  }

  override def reduce(arg: String) =
    reduce(new Argument(arg))

  // @TODO
  override def :+(that: LambdaExpression): LambdaExpression = ???
}

class Argument(val literal: String) extends LambdaVariable {
  override def isEmpty: Boolean = false

  override def toString = literal

  // @TODO add lambda exceptions.
  override def reduce(arg: Argument, boundVariable: String = ""): LambdaExpression =
    throw new Exception("Reduce is unsupported on arguments")

  override def reduce(arg: String): LambdaExpression =
    reduce(new Argument(arg))

  // @TODO
  override def :+(that: LambdaExpression): LambdaExpression = ???
}