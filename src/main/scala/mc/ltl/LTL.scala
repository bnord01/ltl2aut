package mc.ltl
import scala.language.implicitConversions

sealed trait Formula {
  def isElementary = false
  def hasSubformula(x: Formula): Boolean
  def normalForm: Formula
}

sealed trait Elementary extends Formula {
  override def isElementary = true
}

case class AP(s: String) extends Formula with Elementary {
  override def hasSubformula(x: Formula) = false
  override def normalForm                = this
}

case class Not(f: Formula) extends Formula {
  override def isElementary              = f.isElementary
  override def hasSubformula(x: Formula) = x == f || f.hasSubformula(x)
  override def normalForm = f match {
    case AP(s)       => this
    case Not(f1)     => f1.normalForm
    case And(f1, f2) => Or(Not(f1), Not(f2)).normalForm
    case Or(f1, f2)  => And(Not(f1), Not(f2)).normalForm
    case X(f1)       => X(Not(f1)).normalForm
    case U(f1, f2)   => R(Not(f1), Not(f2)).normalForm
    case R(f1, f2)   => U(Not(f1), Not(f2)).normalForm
  }
}

case class And(f1: Formula, f2: Formula) extends Formula {
  override def isElementary = f2 == Not(f1)
  override def hasSubformula(x: Formula) =
    x == f1 || x == f2 || f1.hasSubformula(x) || f2.hasSubformula(x)
  override def normalForm = And(f1.normalForm, f2.normalForm)
}

case class Or(f1: Formula, f2: Formula) extends Formula {
  override def hasSubformula(x: Formula) =
    x == f1 || x == f2 || f1.hasSubformula(x) || f2.hasSubformula(x)
  override def normalForm = Or(f1.normalForm, f2.normalForm)
}

case class U(f1: Formula, f2: Formula) extends Formula {
  override def hasSubformula(x: Formula) =
    x == f1 || x == f2 || f1.hasSubformula(x) || f2.hasSubformula(x)
  override def normalForm = U(f1.normalForm, f2.normalForm)
}

case class R(f1: Formula, f2: Formula) extends Formula {
  override def hasSubformula(x: Formula) =
    x == f1 || x == f2 || f1.hasSubformula(x) || f2.hasSubformula(x)
  override def normalForm = R(f1.normalForm, f2.normalForm)
}

case class X(f1: Formula) extends Formula with Elementary {
  override def hasSubformula(x: Formula) = x == f1 || f1.hasSubformula(x)
  override def normalForm                = X(f1.normalForm)
}

object True extends Or(AP("x"), Not(AP("x")))

object False extends Not(True)

class G(f1: Formula) extends R(False, f1)
object G {
  def apply(f1: Formula) = new G(f1)
}

class F(f1: Formula) extends U(True, f1)
object F {
  def apply(f1: Formula) = new F(f1)
}

object LTLDSL {
  implicit def string2AP(s: String): AP = AP(s)

  implicit class RichFormula(f1: Formula) {
    def until(f2: Formula): U        = U(f1, f2)
    def release(f2: Formula): R      = R(f1, f2)
    def and(f2: Formula): And        = And(f1, f2)
    def or(f2: Formula): Or          = Or(f1, f2)
    def implies(f2: Formula): Or     = Or(Not(f1), f2)
    def orNext(f2: Formula): Or      = Or(f1, X(f2))
    def andNext(f2: Formula): And    = And(f1, X(f2))
    def impliesNext(f2: Formula): Or = Or(Not(f1), X(f2))
    def unary_! : Not                = Not(f1)
  }

  implicit def bool2Formula(b: Boolean): Formula = if (b) True else False

  implicit def string2RichFormula(s: String): RichFormula = RichFormula(AP(s))

}
