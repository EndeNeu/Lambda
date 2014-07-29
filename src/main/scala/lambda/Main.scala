package lambda

import lambda.LambdaBuilder.{AR, BV, EX}
import lambda.core.Lambda._

object Main {

  def main(args: Array[String]) {

    // λx.(λy.y x)
    val exp2 = EX(
      BV(
        "x",
        EX(
          BV(
            "y",
            EX(
              AR("y")
            )
          ),
          AR("x")
        )
      )
    )

    //val exp4: NonEmptyLambdaExpression = identity("x") + identity("y")
    val exp5 =
      EX(
        BV(
          "x",
          EX(
            AR("x")
          )
        ),
        BV(
          "y",
          EX(
            AR("y")
          )
        )
      )

    //println(exp4.LAmbdaExpressions)

    // λx.x
    //    println("λx.x")
    //    println(identity("x").toString)

    // λx.(λy.y x)
    //    println("λx.(λy.y x)")
    //    println(exp2.toString())

    // λx.x λy.y
    //    println("λx.x λy.y")
    //    println(exp5.toString)

    // λx.(x x)
    //    println("λx.(x x)")
    //    println(selfApply("x").toString)

    println("---------------------")

    // λx.x
    println(identity("x").reduce("y").toString)

    // λx.x λy.y
    println(exp5.reduce("z").toString)

    //λx.(x x)
    println(selfApply("x").reduce("z").toString)

    // λx.(λy.y x)
    println(exp2.reduce("z").toString)

    // λx.λy.(x y)
    println(lApply("x", "y"))

  }

}