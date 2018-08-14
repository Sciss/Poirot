/*
 *  Transistors.scala
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

object Transistors extends App with Problem {

  def ntran(b: BooleanVar, x: BooleanVar, y: BooleanVar) = b  #-> (x #= y)
  def ptran(b: BooleanVar, x: BooleanVar, y: BooleanVar) = ~b #-> (x #= y)

  val a     = BooleanVar("a")
  val b     = BooleanVar("b")
  val c     = BooleanVar("c")
  val sum   = BooleanVar("sum")
  val carry = BooleanVar("carry")
  val nca   = BooleanVar("nca")
  val t     = Vec.tabulate(6)(i => BooleanVar("t"+i))
  val q     = Vec.tabulate(4)(i => BooleanVar("q"+i))
  val one   = true  //BooleanVar("1", 1, 1)
  val zero  = false //BooleanVar("0", 0, 0)

  // sum part
  ptran(nca, t(0), one)
  ptran(c, one, t(4))
  ptran(b, t(0), t(4))
  ptran(a, t(0), t(1))
  ptran(nca, t(4), t(1))
  ptran(t(1), one, sum)
  ntran(a, t(1), t(2))
  ntran(nca, t(1), t(5))
  ntran(t(1), sum, zero)
  ntran(b, t(2), t(5))
  ntran(nca, t(2), zero)
  ntran(c, t(5), zero)

  // carry part
  ptran(a, q(0), one)
  ptran(b, q(0), one)
  ptran(a, q(1), one)
  ptran(c, q(0), nca)
  ptran(b, q(1), nca)
  ptran(nca, one, carry)
  ntran(c, nca, q(2))
  ntran(b, nca, q(3))
  ntran(nca, carry, zero)
  ntran(a, q(2), zero)
  ntran(b, q(2), zero)
  ntran(a, q(3), zero)

  val result = satisfyAll(search(List(a, b, c, sum, carry), inputOrder, indomainMin))

  println(a + " " + b + " " + " " + c + " " + " " + sum + " " + " " + carry)
}
