/*
 *  ScalaSolutionListener.scala
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

import org.jacop.search.{Search, SelectChoicePoint, SimpleSolutionListener}
import org.jacop.{core => jc}

/** Solution listener that prints solutions of search
  * using user specified functions.
  */
class ScalaSolutionListener[A <: jc.Var](printFunctions: Seq[() => Unit])
  extends SimpleSolutionListener[A] {

  override def executeAfterSolution(search: Search[A],
                                    select: SelectChoicePoint[A]): Boolean = {
    val res = super.executeAfterSolution(search, select)
    printFunctions.foreach(_.apply())
    res
  }
}
