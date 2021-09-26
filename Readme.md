# ltl2aut

Scala implementation of the LTL2AUT algorithm by Daniele, Giunchiglia and Vardi [[1]](#1-daniele-m-giunchiglia-f-vardi-my-1999) to construct generalised Büchi automata from Linear Time Logic (LTL) formulas, as well as Tauriainen's algorithm for generalised Büchi automata emptiness [[2]](#2-tauriainen-h-2003) as described in [[3]](#3-couvreur-jm-duret-lutz-a-poitrenaud-d-2005).

## Example
Model checking a simple cyclic traffic light for some properties. See [Example.scala](src/main/scala/mc/example/Example.scala)

```scala
  // The set of atomic prepositions
  implicit val atomicPrepositions: Set[AP] = Set("red", "green", "yellow")

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
```

## References

#### [1] Daniele M., Giunchiglia F., Vardi M.Y. (1999) 
Improved Automata Generation for Linear Temporal Logic. In: Halbwachs N., Peled D. (eds) Computer Aided Verification. CAV 1999. Lecture Notes in Computer Science, vol 1633. Springer, Berlin, Heidelberg. https://doi.org/10.1007/3-540-48683-6_23


#### [2] Tauriainen H. (2003)
On translating linear temporal logic into alternating and nondeterministic automata. Research Report A83, Helsinki University of Technology, Laboratory for Theoretical Computer Science, Espoo, Finland (December 2003)

#### [3] Couvreur JM., Duret-Lutz A., Poitrenaud D. (2005)
On-the-Fly Emptiness Checks for Generalized Büchi Automata. In: Godefroid P. (eds) Model Checking Software. SPIN 2005. Lecture Notes in Computer Science, vol 3639. Springer, Berlin, Heidelberg. https://doi.org/10.1007/11537328_15