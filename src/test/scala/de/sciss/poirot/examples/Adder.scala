/*
 *  Adder.scala
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

object Adder extends App with Problem {
  val a     = BooleanVar("a")
  val b     = BooleanVar("b")
  val c     = BooleanVar("c")
  val summa = BooleanVar("summa")
  val carry = BooleanVar("carry")

  // summa part
  summa #= (a ^ b ^ c)

  //carry part
  carry #= ((c & (a ^ b)) | (a & b))

  recordSolutions = true

  val result = satisfyAll(search(List(a, b, c, summa, carry), inputOrder, indomainMin), printTableRow)

  println(s"$a  $b  $c  $summa  $carry")

  def printTableRow(): Unit =
    println(s"${a.value} | ${b.value} | ${c.value} || ${summa.value} | ${carry.value}")
}
