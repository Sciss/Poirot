/*
 *  Quadratic.scala
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

object Quadratic extends App with Problem {

  FloatDomain.setPrecision(1e-12)

  val x = DoubleVar("x", -10, 10)
  val y = DoubleVar("y", -10, 10)

  // constraints
  2.0*x*y + y #= 1.0
  x*y #= 0.2

  val (result, stats) = withStatistics(satisfyAll(searchDouble(List(x,y), inputOrder), () => println(s"$x\n$y")))
  println(stats)
}
