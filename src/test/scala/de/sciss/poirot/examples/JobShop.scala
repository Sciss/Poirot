/*
 *  JobShop.scala
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

import Implicits._

object JobShop extends App with Problem {

  val m = Vec(
    Vec(2,0,1,3,5,4),
    Vec(1,2,4,5,0,3),
    Vec(2,3,5,0,1,4),
    Vec(1,0,2,3,4,5),
    Vec(2,1,4,5,0,3),
    Vec(1,3,5,0,4,2)
  )
  val d = Vec(
    Vec(1,3,6,7,3,6),
    Vec(8,5,10,10,10,4),
    Vec(5,4,8,9,1,7),
    Vec(5,5,5,3,8,9),
    Vec(9,3,5,4,3,1),
    Vec(3,3,9,10,4,1)
  )

  val n = m.length
  val k = m(0).length

  // task start times
  val t = Vec.tabulate(n,k)( (i,j) => IntVar(s"t_${i}_$j", 0, 100))

  // jobs completion time
  val completion = Vec.tabulate(n) (i => t(i)(k-1) + d(i)(k-1))
  val end = max(completion: _*)

  // precedence constraints
  for (i <- 0 until n; j <- 0 until k-1)
    t(i)(j) + d(i)(j) #<= t(i)(j+1)

  // resource constraints
  val maxRes  = n
  val one     = IntVar("1", 1, 1)
  val ones    = Vec.fill(n)(one)

  for ( l <- 0 until maxRes) {
    val TsDs = for {
      i <- 0 until n
      j <- 0 until k
      if m(i)(j) == l
    } yield
      (t(i)(j), IntVar("", d(i)(j), d(i)(j)), one)

    cumulative(TsDs, 1)
  }

  val tList: List[IntVar] = List.tabulate(n,k)((i, j) => t(i)(j)).flatten
  val (result, stats) = withStatistics(minimize(search(tList, smallest, indomainMin), end, printSol))

  println(stats)

  def printSol = () => {
    println(s"\nSolution with cost: ${end.value}\n=======================")
    for (i <- 0 until n)
      println(t(i).toList)
  }
}
