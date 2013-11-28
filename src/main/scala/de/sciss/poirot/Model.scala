package de.sciss.poirot

import scala.collection.mutable
import org.jacop.{core => jc}
import org.jacop.constraints.Constraint

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