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
          AR("x")
        ),
        BV(
          "y",
          AR("y")
        )
      )

    //println(exp4.LAmbdaExpressions)

    // λx.x
    println(identity("x").toString)

    // λx.(λy.y x)
    println(exp2.toString())

    println(exp5.toString)

    /*
    println(exp5.toString)
    println(selfApply("x").toString)
  */

    //println("---------------------")


    /**
     *
     * λy.λx.(y x)
     *
     * λy.λx.(y x) z
     *
     */
    //println(lApply("x", "y"))

    //    println(selfApply("x"))

  }

}