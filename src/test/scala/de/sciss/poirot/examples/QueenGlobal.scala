/*
 *  QueenGlobal.scala
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

object QueenGlobal extends App with Problem {
  val n = 100

  val q = Vec.tabulate(n)(i => IntVar("q" + i, 0, n))
  q.allDifferent()

  val q1 = Vec.tabulate(n)(i => q(i) + i)
  q1.allDifferent()

  val q2 = Vec.tabulate(n)(i => q(i) - i)
  q2.allDifferent()
  
  val success = satisfy(search(q.toList, firstFail, indomainMiddle))

  println(if (success) "Yes" else "No solution")
}
