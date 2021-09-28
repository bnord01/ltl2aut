package mc.ltl2aut
import mc.ltl._

import scala.language.implicitConversions
import scala.language.postfixOps

object LTL2AUT extends Core {

  /**
   * Checks for syntactic implication of formula `f` by the set of formulas `a`
   * 
   * @param f formula to be checked for syntactic implication
   * @param a set of formulas
   * @return `true` if `f` is syntactically implied by `a`
   */
  def synimpl(f: Formula, a: Set[Formula]): Boolean = {
    if (f == True || (a contains f)) return true
    if (f.isElementary) return false
    if (!a.exists(x => x.hasSubformula(f))) return false
    if (alpha1(f) forall (x => synimpl(x, a))) return true
    if (alpha2(f) forall (x => synimpl(x, a))) return true
    return false
  }

  override def contradiction(f: Formula, toCover: Set[Formula], current: Set[Formula], covered: Set[Formula]) =
    synimpl(Not(f).normalForm, toCover ++ current)

  override def redundant(f: Formula, toCover: Set[Formula], current: Set[Formula], covered: Set[Formula]) =
    f match {
      case f if synimpl(f, toCover ++ current)          => true
      case U(f1, f2) if synimpl(f2, toCover ++ current) => true
      case _                                            => false
    }

  override def hasToBeStored(f: Formula) = false

  override def satisfy(s: Set[Formula], f: Formula) = synimpl(f, s)

}

trait Core {

  def hasToBeStored(f: Formula): Boolean
  def contradiction(f: Formula, toCover: Set[Formula], current: Set[Formula], covered: Set[Formula]): Boolean
  def redundant(f: Formula, toCover: Set[Formula], current: Set[Formula], covered: Set[Formula]): Boolean
  def satisfy(s: Set[Formula], f: Formula): Boolean

  def alpha1(f: Formula): Set[Formula] = f match {
    case And(f1, f2) => Set(f1, f2)
    case Or(f1, f2)  => Set(f1)
    case U(f1, f2)   => Set(f2)
    case R(f1, f2)   => Set(f2, f1)
    case _           => Set()
  }
  def alpha2(f: Formula): Set[Formula] = f match {
    case And(f1, f2) => Set(False)
    case Or(f1, f2)  => Set(f2)
    case U(f1, f2)   => Set(f1, X(f))
    case R(f1, f2)   => Set(f2, X(f))
    case _           => Set()
  }

  def cover(
      toCover: Set[Formula],
      current: Set[Formula] = Set(),
      covered: Set[Formula] = Set(),
      theCover: Set[Set[Formula]] = Set()
  ): Set[Set[Formula]] = {
    if (toCover isEmpty)
      return theCover + current
    val f          = toCover.head
    val newToCover = toCover - f
    val newCovered = covered + f
    val newCurrent = if (hasToBeStored(f)) current + f else current
    if (contradiction(f, newToCover, newCurrent, newCovered))
      return theCover
    if (redundant(f, newToCover, newCurrent, newCovered))
      return cover(newToCover, newCurrent, newCovered, theCover)
    if (f isElementary)
      return cover(newToCover, newCurrent + f, newCovered, theCover)
    return cover(
      newToCover ++ (alpha1(f) -- newCurrent),
      newCurrent,
      newCovered,
      cover(newToCover ++ (alpha2(f) -- newCurrent), newCurrent, newCovered, theCover)
    )
  }

  def collectU(f: Formula): Set[U] = f match {
    case X(f1)         => collectU(f1)
    case Not(f1)       => collectU(f1)
    case And(f1, f2)   => collectU(f1) ++ collectU(f2)
    case Or(f1, f2)    => collectU(f1) ++ collectU(f2)
    case R(f1, f2)     => collectU(f1) ++ collectU(f2)
    case f @ U(f1, f2) => collectU(f1) ++ collectU(f2) + f
    case AP(_)         => Set()
  }

  import mc.aut._

  implicit val formulaAPState: APView[Set[Formula]] = (s: Set[Formula]) =>
    new APState {
      def posAP = s.collect { case p: AP => p }
      def negAP = s.collect { case Not(p: AP) => p }
    }

  def createAutomaton(f: Formula): Automata[Set[Formula]] = {
    val I = cover(Set(f.normalForm))
    var U = I
    var Q = I
    var d = Map[Set[Formula], Set[Set[Formula]]]().withDefaultValue(Set())
    while (!(U isEmpty)) {
      val s = U.head
      U = U - s
      for (r <- cover(s collect { case X(m) => m })) {
        if (!(Q contains r)) {
          Q = Q + r
          U = U + r
        }
        d = d + (s -> (d(s) + r))
      }
    }
    var F = Set[Set[Set[Formula]]]()
    for (u <- collectU(f.normalForm)) {
      F += Q.filter(s => !satisfy(s, u) || satisfy(s, u.f2))
    }

    return Automata(Q, I, F, d)
  }

  implicit class formula2Aut(f:Formula) {
    def toAut = LTL2AUT.createAutomaton(f)
  }
}

