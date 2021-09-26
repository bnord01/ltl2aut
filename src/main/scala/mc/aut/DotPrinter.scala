package mc.aut
import scala.language.implicitConversions

implicit class DotPrinter[State: APView](aut: Automata[State]) {
  def dotString: String = {
    import aut._
    var num = Map() ++ (Q.zipWithIndex)
    var fin = Map[State, List[Int]]() withDefaultValue Nil
    for ((f, i) <- F.zipWithIndex; s <- f) {
      fin += s -> (fin(s) :+ i)
    }

    def nodelabel(x: State) =
      if (F.size == 1)
        s"""label="${num(x)}"${if (F.head(x)) ", shape=doublecircle" else ""}"""
      else
        s"""label="${num(x)} (${fin(x) mkString ","})""""

    def init(x: State) = if (I(x)) s"""\ni${num(x)} [style=invisible];\ni${num(x)} -> ${num(x)};""" else ""
    def node(x: State) = s"""${num(x)} [${nodelabel(x)}];${init(x)}"""

    def transition(p: (State, Set[State])) = {
      val (k, v) = p
      v.map(l =>
        s"""${num(k)} -> ${num(l)}[label="{${k.posAP.map(_.s) ++ (k.negAP.map("!" + _.s)) mkString ", "}}"];"""
      )
    }

    s"""digraph gba {
${Q map node mkString "\n"}
${d flatMap transition mkString ("\n")}
}
"""
  }
}
