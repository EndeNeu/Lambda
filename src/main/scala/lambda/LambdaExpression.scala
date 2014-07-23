package lambda


/**
 * λ
 */
trait LambdaExpression {

  def isLambdaExpression: Boolean = true

  def isLambdaVariable = false

  def isEmpty: Boolean

  def reduce[T <: LambdaVariable](arg: Argument): LambdaExpression

}

trait LambdaVariable extends LambdaExpression {

  override def isLambdaVariable = true

  val literal: String

}

class NonEmptyLambdaExpression(val expressions: List[LambdaExpression]) extends LambdaExpression {

  def this(lambda: LambdaExpression) = this(List(lambda))

  override def isEmpty = false

  //  def apply(arg: LambdaExpression) = this.reduce(arg)

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
   * EX(BV, AR, BV, AR)
   *
   *
   * EX(BV("x", AR("x")) x
   *
   * lx.x y
   *
   * lx.(ly.y lz.(z x) x)
   *
   *
   * lx.x -> (x)
   * EX(BV(x, EX(x)) -> (x)
   *
   *
   * @return
   */
  override def reduce[T <: LambdaVariable](arg: Argument): LambdaExpression = {


    expressions.fold(EmptyLambdaExpression)((newEX, currentEX) =>
      currentEX match {
        case bv: BoundVariable =>
          val boundVariable: String = bv.literal
          bv.reduce(arg)
          EmptyLambdaExpression
        case _ => EmptyLambdaExpression


      })

    EmptyLambdaExpression
  }


}

case object Nil extends Nothing {

  // Removal of equals method here might lead to an infinite recursion similar to IntMap.equals.
  override def equals(that: Any) = that match {
    case that1: scala.collection.GenSeq[_] => that1.isEmpty
    case _ => false
  }
}

object EmptyLambdaExpression extends LambdaExpression {

  def isEmpty = true

  def empty: NonEmptyLambdaExpression = new NonEmptyLambdaExpression(List())

  def ++(lambda: LambdaExpression): LambdaExpression = new NonEmptyLambdaExpression(List(lambda))

  override def reduce[T <: LambdaVariable](arg: Argument): LambdaExpression = EmptyLambdaExpression
}


class BoundVariable(val literal: String, val someArgument: List[LambdaExpression]) extends LambdaVariable {

  override def isEmpty = false

  override def toString = someArgument.foldLeft("")((acc, curr) => curr match {
    case bv: BoundVariable => bv.toString
    case arg: Argument =>
      if (argLength > 1 && !acc.contains("(")) acc + "(" + arg.literal + " "
      else if (argLength > 1) acc + arg.literal + ")"
      else acc + arg.literal + " "
  }) + " "

  def argumentsToString =
    if (someArgument.isEmpty) ""
    else if (argLength > 1) "(" + someArgument.map(_.toString).reduce(_ + _).trim + ")"
    else someArgument.map(_.toString).reduce(_ + _)

  def argLength: Int = someArgument.foldLeft(0)((acc, curr) => curr match {
    case ex: NonEmptyLambdaExpression => acc + ex.length
    case arg: Argument => acc + 1
  })

  override def reduce(arg: Argument): LambdaExpression = {

    someArgument.foldLeft(EmptyLambdaExpression.empty)((newLambda, currentLambda) => {

      val t: NonEmptyLambdaExpression = newLambda

      val r = List.empty

      currentLambda match {
        case arg: Argument => newLambda.++(new Argument(arg.literal))
      }
    }
    )


    EmptyLambdaExpression
  }

}

class Argument(val literal: String) extends LambdaVariable {
  override def isEmpty: Boolean = false

  override def toString = literal

  // @TODO add lambda exceptions.
  override def reduce[T <: LambdaVariable](arg: Argument): LambdaExpression =
    throw new Exception("Reduce is unsupported on arguments")

}