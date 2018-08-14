/*
 *  RegularExample.scala
 *  (Poirot)
 *
 *  Copyright (c) 2013-2018 Hanns Holger Rutz. All rights reserved.
 *  Code is often based on or identical to the original JaCoP Scala wrappers by
 *  Krzysztof Kuchcinski and Radoslaw Szymanek.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.poirot
package examples

object RegularExample extends App with Problem {

  val v = List.tabulate(3)(i => IntVar("v" + i, 0, 2))

  var dfa = new FSM(8) // create FSM with eight states

  //   var dfa = new fsm()
  //   for (i <- 0 until 8) dfa += new state()

  dfa.init(dfa(0))
  dfa.addFinalStates(dfa(7))

  dfa(0) ~> (0, dfa(1))
  dfa(0) ~> (1, dfa(2))
  dfa(0) ~> (2, dfa(3))
  dfa(1) ~> (1, dfa(4))
  dfa(1) ~> (2, dfa(5))
  dfa(2) ~> (0, dfa(4))
  dfa(2) ~> (2, dfa(6))
  dfa(3) ~> (0, dfa(5))
  dfa(3) ~> (1, dfa(6))
  dfa(4) ~> (2, dfa(7))
  dfa(5) ~> (1, dfa(7))
  dfa(6) ~> (IntSet(0, 0), dfa(7))

  println(dfa)

  regular(dfa, v)

  val result = satisfyAll(search(v, inputOrder, indomainMin))
}
