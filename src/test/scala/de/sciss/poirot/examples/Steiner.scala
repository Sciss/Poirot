/*
 *  Steiner.scala
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

object Steiner extends App with Problem {
  val n = 7
  val nb = n * (n-1) / 6

  val sets = List.tabulate(nb)(i => new SetVar("set_" + i, 1, n))

  // all sets must have three elements
  sets.foreach(s => card(s) #= 3)

  // there are at most one element common to two sets
  for (i <- 0 until n; j <- i + 1 until n)
    card(sets(i) * sets(j)) #<= 1

  // Symmetry breaking:
  for (i <- 0 until n - 1)
    sets(i) #<= sets(i + 1)

  val result = satisfy(search(sets, inputOrder, indomainMinSet))

  if (result) {
    sets.foreach(si => print(si.dom + " "))
    println() 
  }
  else println("No solution")
}
