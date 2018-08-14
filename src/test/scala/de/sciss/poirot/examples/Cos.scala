/*
 *  Cos.scala
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

object Cos extends App with Problem {

  FloatDomain.setPrecision(1e-12)

  val x = DoubleVar("x", -10, 10)

  x #= cos(x)

  val (result, stats) = withStatistics(satisfyAll(searchDouble(List(x), inputOrder), printValue))
  println(stats)

  def printValue = () =>
    println(s"Value when cos(x)=x is ${x.value}, precision = ${FloatDomain.precision()}")
}
