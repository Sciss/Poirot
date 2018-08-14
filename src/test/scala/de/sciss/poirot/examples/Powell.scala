/*
 *  Powell.scala
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

import de.sciss.poirot.Implicits._
import org.jacop.floats.core.FloatDomain

// Problem 1 from paper "Some tests of Generalized Bisection" by R. Baker Kearfott.

object Powell extends App with Problem {

  FloatDomain.setPrecision(1e-20)

  val x = Vec.tabulate(4)(i => DoubleVar(s"x[$i]", -2, 2))

  // constraint
  x(0) + 10.0*x(1) #= 0.0
  //sum(x, Array[Double](1, 10, 0, 0)) #= 0

  math.sqrt(5.0)*(x(2) - x(3)) #= 0.0

  (x(1) - 2.0*x(2))*(x(1) - 2.0*x(2)) #= 0.0

  math.sqrt(10.0)*(x(0) - x(3))*(x(0) - x(3)) #= 0.0

  val (result, stats) = withStatistics(satisfyAll(searchDouble(x, inputOrder), () => println(x)))
  println(stats)
}
