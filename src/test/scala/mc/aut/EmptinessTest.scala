package mc.aut

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._

import mc.ltl._
import mc.ltl.LTLDSL.string2AP

class EmptinessSpec extends AnyFlatSpec {

  "The empty automaton" should "be empty" in {
    implicit val aps : Set[AP] = Set()
    val a1 = Automata[String](Set(), Set(), Set(), Map())
    a1.isEmpty shouldBe true
  }

  "A simple automaton" should "not be empty" in {
    implicit val aps : Set[AP] = Set("a","b","c")
    val (a, b, c) = ("a", "b", "c")
    val Q1        = Set(a, b, c)
    val I1        = Set(a)
    val F1        = Set(Set(c))
    val d1        = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1        = Automata[String](Q1, I1, F1, d1)

    a1.isEmpty shouldBe false
  }

  "A simple automaton with a non looping accepting state" should "be empty" in {
    implicit val aps : Set[AP] = Set("a","b","c")
    val (a, b, c) = ("a", "b", "c")
    val Q1        = Set(a, b, c)
    val I1        = Set(a)
    val F1        = Set(Set(c))
    val d1        = Map(a -> Set(b, c), b -> Set(b), c -> Set[String]())
    val a1        = Automata[String](Q1, I1, F1, d1)

    a1.isEmpty shouldBe true
  }

  "A simple automaton with two non connected looping accepting sets" should "be empty" in {
    implicit val aps : Set[AP] = Set("a","b","c")
    val (a, b, c) = ("a", "b", "c")
    val Q1        = Set(a, b, c)
    val I1        = Set(a)
    val F1        = Set(Set(b), Set(c))
    val d1        = Map(a -> Set(b, c), b -> Set(b), c -> Set(c))
    val a1        = Automata[String](Q1, I1, F1, d1)

    a1.isEmpty shouldBe true
  }

  "A simple automaton with two connected looping accepting sets" should "not be empty" in {
    implicit val aps : Set[AP] = Set("a","b","c")
    val (a, b, c) = ("a", "b", "c")
    val Q1        = Set(a, b, c)
    val I1        = Set(a)
    val F1        = Set(Set(b), Set(c))
    val d1        = Map(a -> Set(b, c), b -> Set(b, c), c -> Set(c, b))
    val a1        = Automata[String](Q1, I1, F1, d1)

    a1.isEmpty shouldBe false
  }
}
