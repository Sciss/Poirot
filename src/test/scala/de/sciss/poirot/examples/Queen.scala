/*
 *  Queen.scala
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

/** The n-Queens problem is to place n chess queens on an N x N chessboard
  * so that no two queens attack each other.
  */
object Queen extends App with Problem {
  // number of queens, also number of rows and columns of the board
  val n = 50

  // the row of each queen is given by its index,
  // the variable to determine is the queen's column index
  val q = for (i <- 0 until n) yield IntVar("q"+i, 0, n-1)

  // apply constraints
  def noAttack(i: Int, j: Int, qi: IntVar, qj: IntVar): Unit = {
    qi     #!= qj       // two queens cannot occupy the same position
    qi + i #!= qj + j   // they are not in a NW-SE diagonal...
    qi - i #!= qj - j   // ...or a
  }

  for {
    i <- 0   until n
    j <- i+1 until n
  } noAttack(i, j, q(i), q(j))

  val success = satisfy(search(q, firstFail, indomainMiddle))

  val result = if (success) {
    q.map { qi =>
      val pos = qi.value()
      ("." * n).updated(pos, '#').mkString(" ")
    } .mkString("\n")
  } else {
    "No solution!"
  }

  println(result)
}