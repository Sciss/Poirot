/*
 *  Model.scala
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

import org.jacop.constraints.Constraint
import org.jacop.{core => jc}

import scala.collection.mutable

object Model {
  def apply(): Model = new Model
}
/** Manages all variables, constraints and global constraints for JaCoP constraint solver. */
class Model private() extends jc.Store {
  var n = 0

  val constr: mutable.Buffer[Constraint] = new mutable.ListBuffer[Constraint]

  def imposeAllConstraints(): Unit = {
    constr.foreach(impose)
    if (trace) constr.foreach(println)
    constr.clear()
  }
}