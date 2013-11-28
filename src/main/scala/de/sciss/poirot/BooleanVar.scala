package de.sciss.poirot

import org.jacop.{core => jc}
import org.jacop.constraints._

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

  /** Defines equation constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: BooleanVar /* IntVar */): PrimitiveConstraint = {
    val c = new XeqY(this, that)
    model.constr += c
    c
  }

  /** Defines equation constraint a BoolVar and a integer value.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: Boolean /* Int */): PrimitiveConstraint = {
    val b = if (that) 1 else 0
    val c = new XeqC(this, b)
    model.constr += c
    c
  }

  /** Defines logical and (conjunction) constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def & (that: BooleanVar /* IntVar */): BooleanVar = {
    val result      = BooleanVar()
    val parameters  = Array[jc.IntVar](this, that)
    val c           = new AndBool(parameters, result)
    model.constr   += c
    result
  }

  /** Defines logical or (disjunction) constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def | (that: BooleanVar /* IntVar */): BooleanVar = {
    val result      = BooleanVar()
    val parameters  = Array[jc.IntVar](this, that)
    val c           = new OrBool(parameters, result)
    model.constr   += c
    result
  }

  /** Defines logical exclusive or constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def ^ (that: BooleanVar /* IntVar */): BooleanVar = {
    val result    = BooleanVar()
    val params    = Array[jc.IntVar](this, that)
    val c         = new XorBool(params, result)
    model.constr += c
    result
  }

  /** Defines logical negation constraint for BoolVar.
    *
    * @return boolean variable that is the result for this constraint.
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
  def -> (thenConstr: PrimitiveConstraint): Constraint = {
    val c = new IfThen(new XeqC(this, 1), thenConstr)
    model.constr.remove(model.constr.length - 1)
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
    * @param reifC a primitive constraint that is used in reification.
    * @return the defined constraint.
    */
  def <=> (reifC: PrimitiveConstraint): Constraint = {
    val c = new Reified(reifC, this)
    model.constr.remove(model.constr.length - 1)
    model.constr += c
    c
  }
}