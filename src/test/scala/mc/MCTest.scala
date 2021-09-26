package mc
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._

import mc.ltl._
import mc.aut.Automata
import mc.aut.StringAPView

import LTLDSL._

class MCSpec extends AnyFlatSpec {

  "A simple test 1" should "be successful" in {
    implicit val aps: Set[AP] = Set("x")
    val (a, b, c)             = ("a:", "b:", "c:x")
    val Q1                    = Set(a, b, c)
    val I1                    = Set(a)
    val F1                    = Set(Set(c))
    val d1                    = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    val f      = U(True, "x")
    val result = MC(a1, f)
    result should be(true)
  }

  "A simple test 2" should "be successful" in {
    implicit val aps: Set[AP] = Set("x")
    val (a, b, c)             = ("a:!x", "b:!x", "c:x")
    val Q1                    = Set(a, b, c)
    val I1                    = Set(a)
    val F1                    = Set(Set(b))
    val d1                    = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    val f      = U(True, "x")
    val result = MC(a1, f)
    result should be(false)
  }

  "A simple test 3" should "be successful" in {
    implicit val aps: Set[AP] = Set("x", "y")
    val (a, b, c)             = ("a:!x,y", "b:!x,y", "c:x,y")
    val Q1                    = Set(a, b, c)
    val I1                    = Set(a)
    val F1                    = Set(Set(c))
    val d1                    = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    val f      = And(U(True, "x"), R(False, "y"))
    val result = MC(a1, f)
    result should be(true)
  }

  "A simple test 4" should "be successful" in {
    implicit val aps: Set[AP] = Set("x", "y")
    val (a, b, c)             = ("a:!x,y", "b:!x,!y", "c:x,y")
    val Q1                    = Set(a, b, c)
    val I1                    = Set(a)
    val F1                    = Set(Set(c))
    val d1                    = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    val f      = And(U(True, "x"), R(False, "y"))
    val result = MC(a1, f)
    result should be(true)
  }

  "A simple test 5" should "be successful" in {
    implicit val aps: Set[AP] = Set("x", "y")
    val (a, b, c)             = ("a:!x,y", "b:x,!y", "c:x,y")
    val Q1                    = Set(a, b, c)
    val I1                    = Set(a)
    val F1                    = Set(Set(b))
    val d1                    = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    val f      = And(U(True, "x"), R(False, "y"))
    val result = MC(a1, f)
    result should be(false)
  }

  "A traffic light" should "never be green and red" in {
    implicit val aps: Set[AP] = Set("red", "green", "yellow")
    val (a, b, c, d)          = ("a:green", "b:yellow", "c:red", "d:red,yellow")
    val Q1                    = Set(a, b, c, d)
    val I1                    = Set(a)
    val F1                    = Set(Set(a))
    val d1                    = Map(a -> Set(b), b -> Set(c), c -> Set(d), d -> Set(a))
    val a1                    = Automata[String](Q1, I1, F1, d1)

    import LTLDSL._

    MC(a1, G(Not("red" and "green"))) shouldBe true
    MC(a1, F("green")) shouldBe true
    MC(a1, F("red" and "yellow")) shouldBe true
    MC(a1, F("red" and "green")) shouldBe false
    MC(a1, F("red" andNext ("red" and "yellow"))) shouldBe true
    MC(a1, F("yellow" andNext "red")) shouldBe true

    val properties = List(
      G(Not("red" and "green")),
      Not(F("red" and "green")),
      F("green"),
      F("red"),
      F("yellow"),
      G("red" and "yellow" impliesNext "green"),
      G("red" and !"yellow" impliesNext ("red" and "yellow")),
      G("yellow" and !"red" impliesNext "red"),
      G("green" impliesNext "yellow")
    )

    properties forall { MC(a1, _) } shouldBe true
  }

}
