package mc.aut
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._

import mc.ltl.AP
import mc.ltl.LTLDSL.string2AP

class ProdGBASpec extends AnyFlatSpec {
  
  "The Product automaton of two empty automata" should "be empty" in {
    implicit val aps : Set[AP] = Set()
    val a1 = Automata[String](Set(), Set(), Set(), Map())
    val a2 = a1
    val pa = new ProdGBA(a1, a2)
    pa.Q shouldEqual a1.Q
    pa.I shouldEqual a1.I
    pa.F shouldEqual a1.F
    pa.d shouldEqual a1.d
  }

  "The Product automaton with an empty automaton" should "be empty" in {    
    implicit val aps : Set[AP] = Set()
    val a1 = Automata[String](Set(), Set(), Set(), Map())
    val a2 = Automata[String](Set("a,b,c"), Set(), Set(), Map())
    val pa = new ProdGBA(a1, a2)
    pa.Q shouldEqual a1.Q
    pa.I shouldEqual a1.I
    pa.F shouldEqual a1.F
    pa.d shouldEqual a1.d
  }

  "The product automaton of a simple automaton with an identity automaton" should "be correct" in {
    implicit val aps : Set[AP] = Set()
    val (a, b, c) = ("a", "b", "c")
    val x = "x"
    val a1 = Automata[String](Set(a, b, c), Set(a), Set(Set(c)), Map(a -> Set(b, c), b -> Set(b), c -> Set(c)))
    val a2 = Automata[String](Set(x), Set(x), Set(Set(x)), Map(x -> Set(x)))
    val pa = new ProdGBA(a1, a2)
    println("Product:\n" + pa)
    pa.Q shouldEqual Set((a, x), (b, x), (c, x))
    pa.I shouldEqual Set((a, x))
    pa.F shouldEqual Set(Set((c, x)), Set((a, x), (b, x), (c, x)))
    pa.d shouldEqual Map((a, x) -> Set((b, x), (c, x)), (b, x) -> Set((b, x)), (c, x) -> Set((c, x)))
  }

  "The product automaton of two simple automata" should "be correct" in {
    implicit val aps : Set[AP] = Set()
    val (a, b, c) = ("a", "b", "c")
    val Q1 = Set(a, b, c)
    val I1 = Set(a)
    val F1 = Set(Set(c))
    val d1 = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1 = Automata[String](Q1, I1, F1, d1)

    val (x, y) = ("x", "y")
    val Q2 = Set(x, y)
    val I2 = Set(x)
    val F2 = Set(Set(y))
    val d2 = Map(x -> Set(x, y), y -> Set(y))
    val a2 = Automata[String](Q2, I2, F2, d2)

    val pa = new ProdGBA(a1, a2)
    println("Product:\n" + pa)
    pa.Q shouldEqual Set((a, x), (b, x), (c, x), (a, y), (b, y), (c, y))
    pa.I shouldEqual Set((a, x))
    pa.F shouldEqual Set(Set((c, x), (c, y)), Set((a, y), (b, y), (c, y)))
    pa.d shouldEqual Map(
      (a, x) -> Set((b, x), (c, x), (b, y), (c, y)),
      (b, x) -> Set((b, x), (b, y)),
      (c, x) -> Set((c, x), (c, y)),
      (a, y) -> Set((b, y), (c, y)),
      (b, y) -> Set((b, y)),
      (c, y) -> Set((c, y)))
  }
}
