package lambda

import core.LambdaParser
import lambda.LambdaBuilder.{AR, BV, EX}
import lambda.core.Lambda._

object Main {

  def main(args: Array[String]) {

    // λx.(λy.y x)
    val exp2 = EX(
      BV(
        "x",
        BV(
          "y",
          AR("y")
        ),
        AR("x")
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

    val exp6 =
      EX(
        AR("x"),
        BV(
          "y",
          AR("x")
        )
      )


    // λx.x
    println("λx.x")
    println(identity("x").toString)

    // λx.(λy.y x)
    println("λx.(λy.y x)")
    println(exp2.toString())

    // λx.x λy.y
    println("λx.x λy.y")
    println(exp5.toString)

    // λx.(x x)
    println("λx.(x x)")
    println(selfApply("x").toString)

    // x λy.x
    println("x λy.x")
    println(exp6.toString)

    println("---------------------")

    // λx.x -> z
    println("λx.x -> z")
    println(identity("x").betaReduce("z").toString)

    // λx.x λy.y -> z z
    println("λx.x λy.y -> z z")
    println(exp5.betaReduce("z").toString)

    //λx.(x x) -> z z
    println("λx.(x x) -> z z")
    println(selfApply("x").betaReduce("z").toString)

    // λx.(λy.y x) -> λy.y z
    println("λx.(λy.y x) -> λy.y z")
    println(exp2.betaReduce("z").toString)

    // λx.λy.(x y) (z)-> λy.(z y)
    println("λx.λy.(x y) (z)-> λy.(z y)")
    println(lApply("x", "y").betaReduce("z").toString)

    // x λy.x -> x x
    println("x λy.x -> x x")
    println(exp6.betaReduce("z").toString)

    // λy.(y λy.y) (x) -> x λy.y
    val exp = EX(BV("y", AR("y"), BV("y", AR("y"))))
    println("λy.(y λy.y) (x) -> x λy.y")
    println(exp.betaReduce("x"))

    println("---------------------")

    // λy.(y λy.y) (λx.x) -> λx.x λy.y
    println("λy.(y λy.y) (λx.x) -> λx.x λy.y")
    println(exp.betaReduce(EX(BV("x", AR("x")))))

    println("---------------------")

    val test =
      EX(
        AR("x"),
        AR("y"),
        BV("s",
          AR("s")
        ),
        AR("g")
      )

    println("flattens:")
    println("x y λs.s g -> z TODO")
    println(test.flatten().toString)

    println("---------------------")
    println("betaReduce()")

    // λx.x λy.y -> λy.y
    //    println(exp5.betaReduce().toString)

    // λx.x -> λx.x
    //    println(identity("x").betaReduce().toString)

    // λx.λy.(x y) -> λx.λy.(x y)
    //    println(lApply("x", "y").betaReduce().toString)


    // x λx.(λy.y x) λt.t -> x λx.(λy.y λt.t)
    val exp7 =
      EX(
        AR("x"),
        BV(
          "x",
          BV(
            "y",
            AR("y")
          ),
          AR("x")
        ),
        BV(
          "t",
          AR("t")
        )
      )

    println("x λx.(λy.y x) λt.t -> x λy.y λt.t")
    println(exp7.betaReduce().toString)
    println("x λx.(λy.y x) λt.t -> x λt.t")
    println(exp7.betaReduce(true).toString)

    println("---------------------")
    println("string parser")

//    val t: NonEmptyLambdaExpression = LambdaParser.fromString("λx.x")
//    println(t.toString)



//    println(LambdaParser.fromString("λx.(x y)").toString)
    // TODO
    println(LambdaParser.fromString("λx.(λy.(x y))").toString)
    println(LambdaParser.fromString("λx.(λy.x y)").toString)
    println(LambdaParser.fromString("λx.y").toString)

    println("---------------------")
    println("structure")

    println("λx.(λy.(x y))")
    println(LambdaParser.fromString("λx.(λy.(x y))").getStructure())

  }

}