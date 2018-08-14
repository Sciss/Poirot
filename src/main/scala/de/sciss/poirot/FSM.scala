/*
 *  FSM.scala
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

import org.jacop.util.{fsm => jfsm}

import scala.collection.mutable

/** FSM specification for regular constraint. */
class FSM private() extends jfsm.FSM {
  import FSM.State

  val states: mutable.Buffer[State] = mutable.ArrayBuffer.empty

  /** FSM specification for regular constraint.
    *
    * @constructor Creates a new FSM.
    * @param n number of states in this FSM.
    */
  def this(n: Int) = {
    this()
    for (_ <- 0 until n) add(State())
  }

  /** Defines initial state for this FSM.
    *
    * @param s state.
    */
  def init(s: State): Unit = {
    initState = s
    add(s)
  }

  /** Defines a list of final state for this FSM.
    *
    * @param st array of states.
    */
  def addFinalStates(st: State*): Unit = st.foreach(add)

  def add(s: State): Unit = {
    states     += s
    allStates add s
  }

  def += (s: State): this.type = {
    add(s)
    this
  }

  /** Number of states in this FSM. */
  def length: Int = states.length

  /** Gets the state n of this FSM.
    *
    * @param n index of state.
    * @return n-th state
    */
  def apply(n: Int): State = states(n)
}

object FSM {
  def apply() = new FSM

  object State {
    def apply() = new State
  }

  /** state specification for FSM for regular constraint.
    *
    * @constructor Creates a new state for FSM.
    */
  class State private() extends jfsm.FSMState {
    /** Transition of FSM.
      *
      * @param tran values for executing this transition.
      * @param that next state for this transition.
      */
    def ~> (tran: IntSet, that: State): Unit = {
      transitions.add(new jfsm.FSMTransition(tran, that))
    }

    /** Transition of FSM.
      *
      * @param tran integer value for executing this transition.
      * @param that next state for this transition.
      */
    def ~> (tran: Int, that: State): Unit = {
      transitions.add(new jfsm.FSMTransition(IntSet(tran, tran), that))
    }
  }
}