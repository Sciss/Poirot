/*
 *  TinyTSP.scala
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

import org.jacop.floats.core.FloatDomain
import Implicits._

/**
  * A problem defined as in Java based examples.
  *
  * rewriting to Scala by Krzysztof Kuchcinski.
 *
  * @author Krzysztof Kuchcinski and Radoslaw Szymanek
  * @version 4.5
  */
object TinyTSP extends App with Problem {

  FloatDomain.setPrecision(1e-12)

  val N = 4  // number of cities

  val x = Vec(0.0, 1.0, 2.0, 2.0)
  val y = Vec(3.0, 1.0, 2.0, 0.0)
  val d = Vec.tabulate(N,N)( (i,j) => math.sqrt((x(i) - x(j))*(x(i) - x(j)) + (y(i) - y(j)) * (y(i) - y(j))))

  val visit = Vec.tabulate(N)(i => IntVar(s"visit[$i]", 1, N))
  val dist  = Vec.tabulate(N)(i => d(i)(visit(i)))

  val distance = DoubleVar("distance", 0, 1000)
  distance #= sum(dist)

  circuit(visit)

  val (result, stats) = withStatistics(minimize(search(visit, inputOrder, indomainMin), distance, printValue))
  println(stats)

  def printValue = () => {
    print("1 -> ")
    var index = 1
    for (i <- 1 to N) {
      if (i < N)
        print(s"${visit(index-1).value} -> ")
      else
        print(visit(index-1).value)
      index = visit(index-1).value
    }
    println()
    println(distance)
  }
}
