import lambda.LambdaBuilder.{AR, BV, EX}
import lambda.core.Lambda.{lApply, selfApply, identity}
import lambda.core.LambdaParser
import lambda.model.NonEmptyLambdaExpression
import org.specs2.mock.Mockito
import org.specs2.mutable._
import scala.xml.XML

class LambdaTest extends Mockito with Specification {

  // λx.(λy.y x)
  val exp2 = EX(BV("x", BV("y", AR("y")),AR("x")))
  // λy.(y λy.y)
  val exp3 = EX(BV("y", AR("y"), BV("y", AR("y"))))
  // λx.x λy.y
  val exp4: NonEmptyLambdaExpression = identity("x").:+(identity("y"))
  // x λy.x
  val exp6 = EX(AR("x"), BV("y", AR("x")))
  // x λx.(λy.y x) λt.t -> x λx.(λy.y λt.t)
  val exp7 = EX(AR("x"), BV("x", BV("y", AR("y")), AR("x")), BV("t", AR("t")))

  "Lambda constructor" should {
    "construct" in {
      exp2.getStructure().filter(_ != ' ') must beEqualTo("EX(BV(BV(AR)AR))")
      exp3.getStructure().filter(_ != ' ') must beEqualTo("EX(BV(ARBV(AR)))")
      exp4.getStructure().filter(_ != ' ') must beEqualTo("EX(BV(AR)BV(AR))")
      exp6.getStructure().filter(_ != ' ') must beEqualTo("EX(ARBV(AR))")
      selfApply("x").getStructure().filter(_ != ' ') must beEqualTo("EX(BV(ARAR))")
      lApply("x", "y").getStructure().filter(_ != ' ') must beEqualTo("EX(BV(BV(ARAR)))")
      exp7.getStructure().filter(_ != ' ') must beEqualTo("EX(ARBV(BV(AR)AR)BV(AR))")
    }
  }

  "toString" should {
    "stirngify" in {
      exp2.toString must beEqualTo("λx.(λy.y x)")
      exp3.toString must beEqualTo("λy.(y λy.y)")
      exp4.toString must beEqualTo("λx.x λy.y")
      exp6.toString must beEqualTo("x λy.x")
      selfApply("x").toString must beEqualTo("λx.(x x)")
      lApply("x", "y").toString must beEqualTo("λx.λy.(x y)")
      exp7.toString must beEqualTo("x λx.(λy.y x) λt.t")
    }
  }

  "betaReduce" should {
    "reduce" in {
      identity("x").betaReduce("z").toString must beEqualTo("z")
      exp4.betaReduce("z").toString must beEqualTo("z z")
      selfApply("x").betaReduce("z").toString must beEqualTo("z z")
      exp2.betaReduce("z").toString must beEqualTo("λy.y z")
      lApply("x", "y").betaReduce("z").toString must beEqualTo("λy.(z y)")
      exp6.betaReduce("z").toString must beEqualTo("x x")
      exp3.betaReduce(EX(BV("x", AR("x")))).toString must beEqualTo("λx.x λy.y")
      exp7.betaReduce().toString must beEqualTo("x λy.y λt.t")
      exp7.betaReduce(true).toString must beEqualTo("x λt.t")
    }
  }

  "Parser" should {
    "parse lambda expression from string" in {
      LambdaParser.fromString("λx.(λy.(x y))").toString must beEqualTo("λx.λy.(x y)")
      LambdaParser.fromString("λx.(λy.x y)").toString must beEqualTo("λx.(λy.x y)")
      LambdaParser.fromString("λx.y").toString must beEqualTo("λx.y")
    }
  }

}
