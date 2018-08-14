/*
 *  Conference.scala
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

/** Example which solves a simple conference session placement problem.
  * 
  * @author Krzysztof Kuchcinski & Radoslaw Szymanek
  * 
  * It solves a simple conference example problem, where different sessions 
  * must be scheduled according to the specified constraints.
  *
  */
object Conference extends App with Problem {
  // session letter
  // A, B, C, D, E, F, G, H, I, J, K
  // session index number
  // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  val iA = 0; val iB = 1; val iC = 2; val iD = 3; val iE = 4; val iF = 5
  val iG = 6; val iH = 7; val iI = 8; val iJ = 9; val iK = 10

  val sessions = Vec.tabulate(11)(i => IntVar(s"session[$i]", 1, 4))

  // Imposing inequalities constraints between sessions
  // A != J
  sessions(iA) #!= sessions(iJ)
  // I != J
  sessions(iI) #!= sessions(iJ)
  // E != I
  sessions(iE) #!= sessions(iI)
  // C != F
  sessions(iC) #!= sessions(iF)
  // F != G
  sessions(iF) #!= sessions(iG)
  // D != H
  sessions(iD) #!= sessions(iH)
  // B != D
  sessions(iB) #!= sessions(iD)
  // E != K
  sessions(iE) #!= sessions(iK)

  def allDifferent(indices: Int*): Unit = indices.toList.map(sessions).allDifferent()

  // different times - B, G, H, I
  allDifferent(iB, iG, iH, iI)

  // different times - A, B, C, H
  allDifferent(iA, iB, iC, iH)

  // different times - A, E, G
  allDifferent(iA, iE, iG)

  // different times - B, H, K
  allDifferent(iB, iH, iK)

  // different times - D, F, J
  allDifferent(iD, iF, iJ)

  // sessions precedence

  // E < J, D < K, F < K
  sessions(iE) #< sessions(iJ)
  sessions(iD) #< sessions(iK)
  sessions(iF) #< sessions(iK)

  // session assignment
  sessions(iA) #= 1
  sessions(iJ) #= 4

  // There are 3 sessions per half a day, last hald a day only 2
  // Every half a day is a resource of capacity 3, and session J which
  // is assigned the last half a day has a resource requirement 2, others 1.

  val one     = IntVar("one"  , 1, 1)
  val two     = IntVar("two"  , 2, 2)
  val three   = IntVar("three", 3, 3)

  val durations = Vec.fill    (11)(one)
  val resources = Vec.tabulate(11)(i => if (i == iJ) two else one)

  cumulative((sessions, durations, resources).zipped.toList, three)

  val result = satisfyAll(searchSplit(sessions, mostConstrained))
}
