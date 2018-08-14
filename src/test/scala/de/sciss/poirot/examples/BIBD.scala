/*
 *  BIBD.scala
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

import scala.util.control.NonFatal

object BIBD {

  /** Specifies number of rows in the incidence matrix. */
  var v = 7

  /** Specifies number of columns in the incidence matrix. */
  var b = 7

  /** Specifies number of ones in each row. */
  var r = 3

  /** Specifies number of ones in each column. */
  var k = 3

  /** Specifies the value of the scalar product of any two distinct rows. */
  var lambda = 1

  def main(args: Array[String]): Unit = {
    if (args.length > 1) try {
      v = args(0).toInt
      b = args(1).toInt
      r = args(2).toInt
      k = args(3).toInt
      lambda = args(4).toInt
    }
    catch {
      case NonFatal(_) =>
        println("Program parameters if provided must specify v, b, r, k, and lambda")
        sys.exit(1)
    }
    run()
  }

  def run(): Unit = {
    implicit val model: Model = Model()

    val x = List.tabulate(v,b)((i,j) => BooleanVar("x" + i + "_" + j))

    // sum on rows
    for (i <- 0 until v)
      sum(x(i)) #= r // (new IntVar(0,0)/:x(i)) (_ + _)  #= r

    // sum on columns
    for (j <- 0 until b)
      sum(List.tabulate(v)(i => x(i)(j))) #= k

    for ( i <- 0 to v)
      for ( j <- i+1 until v)
        sum(List.tabulate(b)(m => x(i)(m) & x(j)(m))) #= lambda

    val result = satisfy(search(x.flatMap(_.toList), firstFail, indomainMin))

    if (result) {
      for (i <- 0 until v) {
        for (j <- 0 until b)
          print("" + x(i)(j).value + " ")
        println()
      }
    } else {
      println("No solution")
    }
  }
}