package lambda

import lambda.LambdaBuilder.AR


/**
 * λ
 */
trait LambdaExpression {

  def isLambdaExpression: Boolean = true

  def isLambdaVariable = false

  def isEmpty: Boolean

  def reduce[T <: LambdaVariable](arg: T): LambdaExpression

}

trait LambdaVariable extends LambdaExpression {

  override def isLambdaVariable = true

  val literal: String

}

class NonEmptyLambdaExpression(val expressions: List[LambdaExpression]) extends LambdaExpression {

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

  /**
   *
   * @return
   */
  override def toString = expressions.foldLeft("")((acc, current) => current match {
    case bv: BoundVariable => acc + "λ" + bv.literal.toString + "." + bv.argumentsToString
    case ex: NonEmptyLambdaExpression => acc + "(" + ex.toString + ")"
    case arg: Argument => acc + arg.toString + " "
  })

  def length: Int = expressions.length

  /*def stringify(expressions: List[LambdaExpression]): String = expressions match {
    case head :: tail => head match {
      case h: BoundVariable => h.toString + stringify(tail)
      case h: Argument => h.toString + " " + stringify(tail)
      case h: NonEmptyLambdaExpression => "(" + h.toString + stringify(tail) + ")"
    }
    case Nil => ""
  }

  def cycle(expressions: List[LambdaExpression]): String = expressions match {
    case head :: tail => stringify(head) + cycle(tail)
    case Nil => ""
  }

  expressions.toList.fold("": String)({
    case (acc: String, curr: List[NonEmptyLambdaExpression]) => ""
  })

  cycle(expressions.toList)
  */

  /**
   * EX(BV, AR, BV, AR)
   *
   *
   * EX(BV("x", AR("x")) x
   *
   * lx.x y
   *
   * lx.(ly.y lz.(z x) x)
   * @return
   */
  /*
  override def reduce[T <: LambdaVariable](arg: T): LambdaExpression = {
    expressions match {
      case head :: tail => head match {
        case h :: t => h match {
          case bv: BoundVariable => {


            bv.reduce(arg)
          }
        }
      }
    }
    EmptyLambdaExpression
  }
  */
  override def reduce[T <: LambdaVariable](arg: T): LambdaExpression = ???
}

object EmptyLambdaExpression extends LambdaExpression {

  def isEmpty = true

  override def reduce[T <: LambdaVariable](arg: T): LambdaExpression = EmptyLambdaExpression
}


class BoundVariable(x: String, val someArgument: List[LambdaExpression]) extends LambdaVariable {

  override def isEmpty = false

  override val literal: String = x


  /*
  override def toString = {
    val listed: List[LambdaExpression] = flatAndList

    "λ" + x + "." + listed.fold("": String) {
      case (acc: String, current: NonEmptyLambdaExpression) => acc + "(" + current.toString + ")"
      case (acc: String, current: BoundVariable) => acc + current.toString + " "
      case (acc: String, current: Argument) =>
        if (listed.length > 1) acc + "(" + current.toString + " "
        else acc + current.toString + " "
      case (acc: String, current: FreeVariable) =>
        if (listed.length > 1) acc + current.toString + ") "
        else acc + current.toString + ""
    }
  }*/

  override def toString = someArgument.foldLeft("")((acc, curr) => curr match {
    case bv: BoundVariable => bv.toString
    case arg: Argument =>
      if (argLength > 1 && !acc.contains("(")) acc + "(" + arg.literal + " "
      else if (argLength > 1) acc + arg.literal + ")"
      else acc + arg.literal + " "
  }) + " "

  def argumentsToString =
    if (someArgument.isEmpty) ""
    else if(argLength > 1) "(" + someArgument.map(_.toString).reduce(_ + _) + ")"
    else someArgument.map(_.toString).reduce(_ + _)

  def argLength: Int = someArgument.foldLeft(0)((acc, curr) => curr match {
    case ex: NonEmptyLambdaExpression => acc + ex.length
    case arg: Argument => acc + 1
  })

  override def reduce[T <: LambdaVariable](arg: T) = EmptyLambdaExpression

  def replace(x: String) = {

    def iterate(expression: List[LambdaExpression]) = expression match {
      case head :: tail => head match {
        case arg: Argument => expression.updated(expression.indexOf(head), AR(x))
      }
    }

    def cycle(expressions: List[List[LambdaExpression]]): Seq[List[LambdaExpression]] = {
      expressions match {
        case head :: tail => expressions.updated(expressions.indexOf(head), iterate(head))
      }
    }

    //cycle(someArgument.toList)

  }

}

class Argument(x: String) extends LambdaVariable {
  override def isEmpty: Boolean = false

  override def toString = x

  override def reduce[T <: LambdaVariable](arg: T): LambdaExpression = ???

  override val literal: String = x
}