/*
 *  BooleanVar.scala
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

import org.jacop.constraints._
import org.jacop.{core => jc}

object BooleanVar {
  /** Creates a new boolean variable.
    *
    * @param name variable's identifier.
    */
  def apply(name: String)(implicit model: Model): BooleanVar = {
    val res = new BooleanVar(name, 0, 1)
    model.n += 1
    res
  }

  /** Convenience constructor which creates a boolean variable
    * reifying the given constraint.
    *
    * This a shortcut for
    * {{
    * val b = BooleanVar()
    * b #<-> constr
    * b
    * }}
    *
    * @param constr the constraint which will be removed from its model and replaced with a reified constraint
    */
  def apply(constr: PrimitiveConstraint)(implicit model: Model): BooleanVar = {
    val b = apply()(model)
    b #<-> constr
    b
  }

  /** Creates a new boolean variable, using an automatically generated name. */
  def apply()(implicit model: Model): BooleanVar = apply("_$" + model.n)
}
/** Defines a boolean variable and its primitive constraints.
  *
  * @constructor  Creates a new boolean variable.
  * @param name   variable's identifier.
  * @param min    minimal value for variable's domain.
  * @param max    maximal value for variable's domain.
  */
class BooleanVar private[poirot](name: String, min: Int, max: Int)(implicit model: Model)
  extends IntVar(name, min, max) {

   /* Defines an anonymous boolean variable.
    *
    * @constructor Creates a new boolean variable.
    * @param l minimal value for variable's domain.
    * @param r maximal value for variable's domain.
    */
  private[poirot] def this(l: Int, r: Int)(implicit model: Model) = {
    this("_$" + model.n, l, r)
    model.n += 1
  }

  /** Defines an 'equation' constraint between two `BooleanVar` instances.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: BooleanVar /* IntVar */): PrimitiveConstraint = {
    val c = new XeqY(this, that)
    model.constr += c
    c
  }

  /** Defines an 'equation' constraint between this variable and a `Boolean` value.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: Boolean /* Int */): PrimitiveConstraint = {
    val b = if (that) 1 else 0
    val c = new XeqC(this, b)
    model.constr += c
    c
  }

  /** Defines a logical 'and' (conjunction) constraint between two `BooleanVar` instances.
    *
    * @param that a second parameter for the 'and' constraint.
    * @return the defined constraint.
    */
  def & (that: BooleanVar /* IntVar */): BooleanVar = {
    val result      = BooleanVar()
    val parameters  = Array[jc.IntVar](this, that)
    val c           = new AndBool(parameters, result)
    model.constr   += c.decompose(store).get(0)
    result
  }

  /** Defines a logical 'or' (disjunction) constraint between two `BooleanVar` instances.
    *
    * @param that a second parameter for the 'or' constraint.
    * @return the defined constraint.
    */
  def | (that: BooleanVar /* IntVar */): BooleanVar = {
    val result      = BooleanVar()
    val parameters  = Array[jc.IntVar](this, that)
    val c           = new OrBool(parameters, result)
    model.constr   += c.decompose(store).get(0)
    result
  }

  /** Defines a logical 'exclusive or' constraint between two `BooleanVar` instances.
    *
    * @param that a second parameter for the 'exclusive or' constraint.
    * @return the defined constraint.
    */
  def ^ (that: BooleanVar /* IntVar */): BooleanVar = {
    val result    = BooleanVar()
    val params    = Array[jc.IntVar](this, that)
    val c         = new XorBool(params, result)
    model.constr += c
    result
  }

  /** Defines a logical 'negation' constraint for this variable.
    *
    * @return variable that is the result for this constraint.
    */
  def unary_~ : BooleanVar = {
    val result    = BooleanVar()
    val c         = new XplusYeqC(this, result, 1)
    model.constr += c
    result
  }

  /** Defines an implication constraint.
    *
    * Note: this assumes that the `thenConstr` posts to the model. The method then
    * removes that posted constraint and replaced it by an amended version.
    * XXX TODO: this is ugly. A better solution would be to have `thenConstr` be
    * a call-by-name parameter and push a temporary model instead?
    *
    * @param thenConstr a primitive constraint that will hold if this variable is 1.
    * @return the defined constraint.
    */
  def #-> (thenConstr: PrimitiveConstraint): Constraint = {
    val c   = new IfThen(new XeqC(this, 1), thenConstr)
    val old = model.constr.remove(model.constr.length - 1)
    if (old != thenConstr)
      throw new IllegalStateException(s"Last constraint to model ($old) must be the implied constraint ($thenConstr)")
    model.constr += c
    c
  }

  /** Defines a reified constraint.
    *
    * Note: this assumes that the `thenConstr` posts to the model. The method then
    * removes that posted constraint and replaced it by an amended version.
    * XXX TODO: this is ugly. A better solution would be to have `thenConstr` be
    * a call-by-name parameter and push a temporary model instead?
    *
    * @param reifiedConstr  a primitive constraint that is used in reification.
    * @return the defined constraint.
    */
  def #<-> (reifiedConstr: PrimitiveConstraint): Constraint = {
    val c   = new Reified(reifiedConstr, this)
    val old = model.constr.remove(model.constr.length - 1)
    if (old != reifiedConstr)
      throw new IllegalStateException(s"Last constraint to model ($old) must be the constraint to be reified ($reifiedConstr)")
    model.constr += c
    c
  }
}
