package mc

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._

import org.scalacheck.Prop
import org.scalatestplus.scalacheck.Checkers
import org.scalatestplus.scalacheck.Checkers._
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Gen

import mc.ltl._
import mc.ltl2aut.LTL2AUT
import mc.aut.Automata

class MCCheckSpec extends AnyFlatSpec {

  /* Generator for LTL Formula */
  implicit def arbLTL: Arbitrary[Formula] =
    Arbitrary {
      val genAP = for (s <- Gen.alphaLowerStr suchThat (_ != "")) yield AP(s)

      def genTF: Gen[Formula] = Gen.oneOf(mc.ltl.True, mc.ltl.False)

      def genFormula1(sz: Int): Gen[Formula] = for {
        f <- sizedFormula(sz - 1)
        n <- Gen.oneOf(X.apply, Not.apply)
      } yield n(f)

      def genFormula2(sz: Int): Gen[Formula] = for {
        f1 <- sizedFormula(sz / 2 - 1)
        f2 <- sizedFormula(sz / 2 - 1)
        n <- Gen.oneOf(U.apply, R.apply, And.apply, Or.apply)
      } yield n(f1, f2)

      def sizedFormula(sz: Int) =
        if (sz <= 0) Gen.oneOf(genAP, genTF)
        else Gen.frequency((1, genAP), (1, genTF), (2, genFormula1(sz)), (3, genFormula2(sz)))

      Gen.sized(sz => sizedFormula(sz))
    }

  "Generating LTL Formula" should "work" in {
    check((f: Formula) => true, minSuccessful(10))
  }

  "Automata created for Formula" should "be well formed" in {
    import mc.ltl2aut.LTL2AUT.formula2Aut
    check((f: Formula) => wellFormed(f.toAut), minSuccessful(100))
  }

  def wellFormed[T](aut: Automata[T]): Prop = {
    import aut._
    ("Initial states are states" |: (I subsetOf Q)) &&
      ("Final state sets are states" |: (F forall {
        _ subsetOf Q
      })) &&
      ("Transitions are valid " |: (d forall { (s, S) => Q(s) && (S subsetOf Q) })) &&
      ("All have transitions " |: (Q forall {
        d(_) != null
      }))
  }

  "LTL Formula" should "be satified by their automata" in {
    import mc.ltl2aut.LTL2AUT.formulaAPState
    import mc.ltl2aut.LTL2AUT.formula2Aut
    check((f: Formula) => MC(f.toAut, f), minSuccessful(15))
  }

  "Nontivial negated LTL Formula" should "not be satified by their automata" in {
    import mc.ltl2aut.LTL2AUT.formulaAPState
    import mc.ltl2aut.LTL2AUT.formula2Aut
    import mc.aut._
    check((f: Formula) =>
      f.toAut.isEmpty || !MC(f.toAut, Not(f))
      , minSuccessful(15))
  }
}
