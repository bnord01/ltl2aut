package mc.example
import mc.MC
import mc.aut._
import mc.ltl.LTLDSL._
import mc.ltl._
import mc.ltl2aut.LTL2AUT
import mc.ltl2aut.LTL2AUT.formulaAPState

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import scala.language.postfixOps

object Example extends App {

  // The set of atomic prepositions
  implicit val atomicPropositions: Set[AP] = Set("red", "green", "yellow")

  // Constructing an automaton representing a simple traffic light
  val (a, b, c, d)       = ("a:green", "b:yellow", "c:red", "d:red,yellow")
  val states             = Set(a, b, c, d)
  val initialStates      = Set(a)
  val acceptingStateSets = Set(Set(a))
  val transitions        = Map(a -> Set(b), b -> Set(c), c -> Set(d), d -> Set(a))
  val trafficLight       = Automata[String](states, initialStates, acceptingStateSets, transitions)

  // A simple property, it should generally not be the case that the traffic light is red and green
  val never_green_and_red = G(Not("red" and "green"))

  // Model check the traffic light for the property
  assert(MC(trafficLight, never_green_and_red))

  // Some other properties
  val properties = List(
    G(Not("red" and "green")),                               // as above
    Not(F("red" and "green")),                               // the dual form
    F("green"),                                              // at some point the traffic light will be green
    F("red"),                                                // at some point the traffic light will be red
    F("yellow"),                                             // at some point the traffic light will be yellow
    F("red" and "yellow"),                                   // at some point the traffic light will be red and yellow
    G("red" and "yellow" impliesNext "green"),               // red&yellow -> green
    G("red" and !"yellow" impliesNext ("red" and "yellow")), // red&!yellow -> red&yellow
    G("yellow" and !"red" impliesNext "red"),                // yellow&!red -> red
    G("green" impliesNext "yellow")                          // green -> yellow
  )

  // Check the traffic light for all properties
  assert(properties forall { MC(trafficLight, _) })

  // Print some automata to dot

  val frmlAut = mc.ltl2aut.LTL2AUT.createAutomaton(Not(never_green_and_red))
  val prodAut = new ProdGBA(trafficLight, frmlAut)

  Files.write(Paths.get("pa.dot"), prodAut.dotString.getBytes(StandardCharsets.UTF_8))
  Files.write(Paths.get("aut.dot"), trafficLight.dotString.getBytes(StandardCharsets.UTF_8))
  Files.write(Paths.get("frml.dot"), frmlAut.dotString.getBytes(StandardCharsets.UTF_8))

  //import sys.process._
  //"dot -Tpdf -O pa.dot" !
  //"dot -Tpdf -O aut.dot" !
  //"dot -Tpdf -O frml.dot" !

}
