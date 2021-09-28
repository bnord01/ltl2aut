package mc

import mc.aut.Automata
import mc.ltl.Formula
import mc.ltl.Not
import mc.ltl2aut.LTL2AUT

object MC {

  import mc.aut._
  import mc.ltl2aut.LTL2AUT.formulaAPState

  def apply[S: APView](a: Automata[S], f: Formula): Boolean = new ProdGBA(a, LTL2AUT.createAutomaton(Not(f))).isEmpty
}
