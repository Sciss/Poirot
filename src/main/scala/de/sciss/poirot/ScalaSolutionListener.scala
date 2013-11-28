package de.sciss.poirot

import org.jacop.{core => jc}
import org.jacop.search.{Search, SelectChoicePoint, SimpleSolutionListener}

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
