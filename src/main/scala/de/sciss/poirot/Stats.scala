/*
 *  Stats.scala
 *  (Poirot)
 *
 *  Copyright (c) 2013-2020 Hanns Holger Rutz. All rights reserved.
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

case class Stats(nodes: Int, decisions: Int, wrong: Int, backtracks: Int, depth: Int, solutions: Int) {
  override def toString: String = "Search statistics:\n==================" +
    "\n Search nodes           : " + nodes +
    "\n Search decisions       : " + decisions +
    "\n Wrong search decisions : " + wrong +
    "\n Search backtracks      : " + backtracks +
    "\n Max search depth       : " + depth +
    "\n Number solutions       : " + solutions
}