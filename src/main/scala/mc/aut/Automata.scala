package mc.aut

import mc.ltl.AP

import scala.language.implicitConversions

trait APState {
  def posAP: Set[AP]
  def negAP: Set[AP]
}

type APView[S] = Conversion[S, APState]

case class Automata[S: APView](Q: Set[S], I: Set[S], F: Set[Set[S]], d: Map[S, Set[S]]) {

  override def toString(): String =
    s"""GBA(
       |Q:(${Q.mkString(",\n     ")})
       |I:(${I.mkString(",\n     ")})
       |F:(${F.size}: ${F.map("( " + _.mkString(", ") + " )").mkString(",\n     ")})
       |d:(${d.map { case (k, v) => k.toString + " -> " + v.mkString(", ") }.mkString(",\n     ")})
       |)""".stripMargin
}

implicit def prodAPView[S1: APView, S2: APView]: APView[(S1, S2)] = (s: (S1, S2)) =>
  new APState {
    def posAP = s._1.posAP ++ s._2.posAP
    def negAP = s._1.negAP ++ s._2.negAP
  }

def compat[A: APView, B: APView](a: A, b: B) =
  !(a.posAP exists b.negAP) && !(a.negAP exists b.posAP)

def cp[A: APView, B: APView](s1: Set[A], s2: Set[B]): Set[(A, B)] =
  for (x <- s1; y <- s2 if compat(x, y)) yield (x, y)

def prodAcc[A: APView, B: APView](q1: Set[A], q2: Set[B], f1: Set[Set[A]], f2: Set[Set[B]]): Set[Set[(A, B)]] =
  (for (as <- f1) yield cp(as, q2)) ++ (for (bs <- f2) yield cp(q1, bs))

def prodTrans[A: APView, B: APView](d1: Map[A, Set[A]], d2: Map[B, Set[B]]): Map[(A, B), Set[(A, B)]] =
  for ((k1, v1) <- d1; (k2, v2) <- d2 if compat(k1, k2)) yield (k1, k2) -> cp(v1, v2)

class ProdGBA[S1, S2](a1: Automata[S1], a2: Automata[S2])(implicit ev1: APView[S1], ev2: APView[S2])
    extends Automata[(S1, S2)](
      cp(a1.Q, a2.Q),
      cp(a1.I, a2.I),
      prodAcc(a1.Q, a2.Q, a1.F, a2.F),
      prodTrans(a1.d, a2.d)
    )(prodAPView(ev1, ev2))


@Deprecated("Use StringAPView instead")
/*implicit*/ val stringAPView : APView[String] = (s: String) => new APState {
    val pos = "(?<=^|,)([a-zA-Z0-9]+)(?=,|$)".r
    val neg = "(?<=^!|,!)([a-zA-Z0-9]+)(?=,|$)".r
    def posAP = Set[AP]() ++ (pos findAllIn s map { p => AP(p) })
    def negAP = Set[AP]() ++ (neg findAllIn s map { p => AP(p) })
  }



implicit class StringAPView(implicit aps:Set[AP]) extends APView[String] {    
  val pos = "(?<=^|,|:)([a-zA-Z0-9]+)(?=,|$)".r    
  def apply(str:String) = new APState {
    def posAP = Set[AP]() ++ (pos findAllIn str map { p => AP(p) })
    def negAP = aps -- (pos findAllIn str map { p => AP(p) })  
  }
}