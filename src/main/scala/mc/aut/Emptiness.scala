package mc.aut

import mc.ltl.AP

import scala.language.implicitConversions

implicit class AutomataEmptiness[State](aut: Automata[State]) {

  def isEmpty(s0: State): Boolean = {
    // Source: http://spinroot.com/spin/symposia/ws05/019_paper.pdf Figure 6.
    import aut._
    require(F.size > 0)
    type AC = Set[State]
    var H      = Map[State, (Boolean, Set[AC])]()
    var W      = Map[State, Map[AC, Int]]()
    var weight = Map[AC, Int]() withDefaultValue 0

    def propagate(s: State, acc: Set[AC], t: State): Boolean = {
      val (tcol, tacc) = H(t)
      if (tcol && F == (acc ++ tacc ++ H(s)._2 ++ F.filter { f => weight(f) > W(t)(f) })) return false
      else if (!(acc subsetOf tacc)) {
        H += t -> ((tcol, tacc ++ acc))
        if (!dfs_red(t, acc))
          return false
      }
      return true
    }

    def dfs_red(s: State, acc: Set[AC]): Boolean = {
      for (t <- d(s)) {
        if (H.keySet(t) && !propagate(s, acc, t))
          return false
      }
      return true
    }

    def dfs_blue(s: State): Boolean = {
      H += s -> ((true, Set()))
      W += s -> weight
      for (t <- d(s)) {
        if (!H.keySet(t)) {
          for (f <- F if f(t)) weight += f -> (weight(f) + 1)
          if (!dfs_blue(t))
            return false
          for (f <- F if f(t)) weight += f -> (weight(f) - 1)
        }
        if (!propagate(s, H(s)._2 ++ F.filter(_(t)), t))
          return false
      }
      H += s -> ((false, H(s)._2))
      return true
    }

    dfs_blue(s0)
  }

  def isEmpty: Boolean = {
    for (s <- aut.I) {
      if (!isEmpty(s)) return false
    }
    return true
  }

}
