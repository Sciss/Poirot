/*
 *  GraphColoring.scala
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

object GraphColoring extends App with Problem {
  val size = 4
  val v = Vec.tabulate(size)(i => IntVar("v"+i, 1, size))

  v.foreach(x => println(x))

  v(0) #!= v(1)
  v(0) #!= v(2)
  v(1) #!= v(2)
  v(1) #!= v(3)
  v(2) #!= v(3)

  val (result, stats) = withStatistics(satisfy(search(v, inputOrder, indomainMin), printSol))
  println(stats)

  if (result)
    println("*** After Search: " + v(0)+", "+v(1) +", "+ v(2) +", "+v(3))
  else
    println("*** No")

  def printSol = () => {
    println("Solution: " + v.toList)
  }

}
