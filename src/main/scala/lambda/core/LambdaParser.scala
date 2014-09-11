package lambda.core

import lambda.model.{NonEmptyLambdaExpression, Argument, LambdaExpression, BoundVariable}

object LambdaParser {

  // split all spaces which are not inside ()
  val regex = " (?![^(]*\\))"

  def fromString(expression: String) = new NonEmptyLambdaExpression(parseString(expression))

  // λx.x
  private[this] def parseString(expression: String): List[LambdaExpression] = {
    val splitted: Array[String] = expression.split(regex)
    splitted.map(buildLambda).toList
  }

  private[this] def buildLambda(expression: String): LambdaExpression = {
//    println("exp")
//    println(expression)
    if (expression.startsWith("λ"))
      new BoundVariable(expression.tail.head.toString, parseString(parseBoundVariableArgument(expression.split("\\.", 2).tail)))
    else
      new Argument(expression(0).toString)
  }

  def parseBoundVariableArgument(arguments: Array[String]): String = {
    if (arguments.head.length == 1 || arguments.head == "λ") arguments.head
    else arguments.head.drop(1).dropRight(1)
  }

}

/*

scala> val str = "λy.(y λy.y) λx.x"
str: String = λy.(y λy.y) λx.x

scala> str.split(" (?![^(]*\\))")
res20: Array[String] = Array(λy.(y λy.y), λx.x)

 */