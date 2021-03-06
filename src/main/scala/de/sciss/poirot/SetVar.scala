/*
 *  SetVar.scala
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
import org.jacop.set.constraints._
import org.jacop.{core => jc, set => jset}

/** Defines a set variable and its primitive constraints.
  *
  * @constructor Creates a new set variable.
  * @param name variable's identifier.
  * @param glb greatest lower bound for variable's domain.
  * @param lub least upper bound on variable's domain.
  */
class SetVar(name: String, glb: Int, lub: Int)(implicit model: Model)
  extends jset.core.SetVar(model, name, glb, lub) {

  /** Defines an anonymous set variable.
    *
    * @constructor Creates a new set variable.
    * @param glb greatest lower bound for variable's domain.
    * @param lub least upper bound on variable's domain.
    */
  def this(glb: Int, lub: Int)(implicit model: Model) = {
    this("_$" + model.n, glb, lub)
    model.n += 1
  }

  /** Defines an anonymous set variable with maximal set domain.
    *
    * @constructor Creates a new finite domain integer variable.
    */
  def this()(implicit model: Model) = {
    this("_$" + model.n, jc.IntDomain.MinInt, jc.IntDomain.MaxInt)
    model.n += 1
  }

  /** Defines set intersection constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result set variable that is the result for this constraint.
    */
  def * (that: SetVar): SetVar = {
    val result = new SetVar()
    val c = new AintersectBeqC(this, that, result)
    model.constr += c
    result
  }

  /** Defines set union constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result set variable that is the result for this constraint.
    */
  def + (that: SetVar): SetVar = {
    val result = new SetVar()
    val c = new AunionBeqC(this, that, result)
    model.constr += c
    result
  }

  /** Defines set subtraction constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result set variable that is the result for this constraint.
    */
  def \ (that: SetVar): SetVar = {
    val result = new SetVar()
    val c = new AdiffBeqC(this, that, result)
    model.constr += c
    result
  }

  /** Defines set disjoint constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def <> (that: SetVar): Constraint = {
    val c = new AdisjointB(this, that)
    model.constr += c
    c
  }

  /** Defines set inclusion constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def in(that: SetVar): Constraint = {
    val c = new AinB(this, that)
    model.constr += c
    c
  }

  /** Defines set inclusion constraint between a set variables and a set.
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def in(that: IntSet): Constraint = {
    val c = new AinS(this, that)
    model.constr += c
    c
  }

  /** Defines set equality constraint between two set variables.
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def #= (that: SetVar): Constraint = {
    val c = new AeqB(this, that)
    model.constr += c
    c
  }

  /** Defines set equality constraint between a set variable and a set.
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def #= (that: IntSet): Constraint = {
    val c = new AeqS(this, that)
    model.constr += c
    c
  }

  /** Defines constraint this ordered set is lexicographically greater or equal than set "that".
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def #>= (that: SetVar): Constraint = {
    val c = new jset.constraints.Lex(that, this)
    model.constr += c
    c
  }

  /** Defines constraint this ordered set is lexicographically less or equal than set "that".
    *
    * @param that second parameter for the constraint.
    * @return result this constraint.
    */
  def #<= (that: SetVar): Constraint = {
    val c = new jset.constraints.Lex(this, that)
    model.constr += c
    c
  }
}
