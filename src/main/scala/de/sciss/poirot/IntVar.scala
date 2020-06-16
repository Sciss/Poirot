/*
 *  IntVar.scala
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

import org.jacop.constraints._
import org.jacop.core.IntDomain
import org.jacop.set.constraints.{EinA, XinA}
import org.jacop.{core => jc}

object IntVar {
  def apply(name: String, min: Int, max: Int)(implicit model: Model): IntVar =
    new IntVar(name, min, max)

  /** Creates a new finite domain integer variable.
    *
    * @param min minimal value of variable's domain.
    * @param max maximal value of variable's domain.
    */
  def apply(min: Int, max: Int)(implicit model: Model): IntVar = {
    val res = apply("_$" + model.n, min, max)
    model.n += 1
    res
  }

  /** Creates a new finite domain integer variable with minimal and maximal
    * values in the domain defined by JaCoP.
    *
    * @param name variable's identifier.
    */
  def apply(name: String)(implicit model: Model): IntVar = {
    val res = apply(name, jc.IntDomain.MinInt, jc.IntDomain.MaxInt)
    model.n += 1
    res
  }

  /** Creates a new finite domain integer variable with minimal and maximal
    * values in the domain defined by JaCoP.
    */
  def apply()(implicit model: Model): IntVar = {
    val res = apply(jc.IntDomain.MinInt, jc.IntDomain.MaxInt)
    model.n += 1
    res
  }

  /** Creates a new finite domain integer variable with the domain defined by IntSet.
    *
    * @param dom variable's domain defined as a set of integers (IntSet).
    */
  def apply(dom: IntSet)(implicit model: Model): IntVar = {
    val res = apply()
    res.dom.intersectAdapt(dom)
    model.n += 1
    res
  }

  /** Creates a new finite domain integer variable with the domain defined by IntSet.
    *
    * @param name variable's identifier.
    * @param dom  variable's domain defined as a set of integers (IntSet).
    */
  def apply(name: String, dom: IntSet)(implicit model: Model): IntVar = {
    val res = apply(name)
    res.dom.intersectAdapt(dom)
    model.n += 1
    res
  }
}
/** Defines a finite domain integer variable and its primitive constraints.
  */
class IntVar protected (name: String, min: Int, max: Int)(implicit model: Model)
  extends jc.IntVar(model, name, min, max) {

  private def safeNeg(in: Int): Int =
    if (in == Int.MinValue) Int.MaxValue else -in

  /** Defines an 'add' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the addition constraint.
    * @return IntVar variable being the result of the addition constraint.
    */
  def + (that: IntVar): IntVar = {
    import IntDomain.addInt
    val result    = IntVar(addInt(this.min(), that.min()), addInt(this.max(), that.max()))
    val c         = new XplusYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines an 'add' constraint between this `IntVar` and an integer value.
    *
    * @param that a second integer parameter for the addition constraint.
    * @return IntVar variable being the result of the addition constraint.
    */
  def + (that: Int): IntVar = {
    import IntDomain.addInt
    val result    = IntVar(addInt(this.min(), that), addInt(this.max(), that))
    val c         = new XplusCeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'subtract' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the subtraction constraint.
    * @return IntVar variable being the result of the subtraction constraint.
    */
  def - (that: IntVar): IntVar = {
    import IntDomain.subtractInt
    val result    = IntVar(subtractInt(this.min(), that.max()),
                           subtractInt(this.max(), that.min()))
    val c         = new XplusYeqZ(result, that, this)
    model.constr += c
    result
  }

  /** Defines a 'subtract' constraint between this variable and an integer value.
    *
    * @param that a second integer parameter for the 'subtract' constraint.
    * @return variable being the result of the constraint.
    */
  def - (that: Int): IntVar = {
    import IntDomain.subtractInt
    val result    = IntVar(subtractInt(this.min(), that), subtractInt(this.max(), that))
    val c         = new XplusCeqZ(result, that, this)
    model.constr += c
    result
  }

  /** Defines a 'multiplication' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the 'multiplication' constraint.
    * @return variable being the result of the constraint.
    */
  def * (that: IntVar): IntVar = {
    val bounds    = IntDomain.mulBounds(this.min(), this.max(), that.min(), that.max())
    val result    = IntVar(bounds.min, bounds.max)
    val c         = new XmulYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'multiplication' constraint between this variable and an integer value.
    *
    * @param that a second integer parameter for the 'multiplication' constraint.
    * @return variable being the result of the constraint.
    */
  def * (that: Int): IntVar = {
    val bounds    = IntDomain.mulBounds(this.min(), this.max(), that, that)
    val result    = IntVar(bounds.min, bounds.max)
    val c         = new XmulCeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines integer division constraint between two IntVar.
    *
    * @param that a second parameter for the integer division constraint.
    * @return IntVar variable being the result of the integer division constraint.
    */
  def / (that: IntVar): IntVar = {
    val bounds    = IntDomain.divBounds(this.min(), this.max(), that.min(), that.max())
    val result    = IntVar(bounds.min, bounds.max)
    val c         = new XdivYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines a constraint for integer reminder from division between two `IntVar` instances.
    *
    * @param that a second parameter for constraint.
    * @return variable being the result of the constraint.
    */
  def % (that: IntVar): IntVar = {
    import math.{min => mmin, max => mmax, abs => mabs}
    
    val (remMin, remMax) = if (this.min() >= 0) {
      (0,  mmax(mabs(that.min()), mabs(that.max())) - 1)
    } else if (this.max() < 0) {
      (0, -mmax(mabs(that.min()), mabs(that.max())) + 1)
    } else {
      (mmin(mmin(that.min(),-that.min()), mmin(that.max(),-that.max())) + 1,
       mmax(mmax(that.min(),-that.min()), mmax(that.max(),-that.max())) - 1)
    }

    val result    = IntVar(remMin, remMax)
    val c         = new XmodYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines an 'exponentiation' constraint between two `IntVar` instances.
    *
    * @param that exponent for the 'exponentiation' constraint.
    * @return variable being the result of the constraint.
    */
  def pow(that: IntVar): IntVar = {
    val result    = IntVar()
    val c         = new XexpYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines an unary "-" constraint for this variable.
    *
    * @return the defined constraint.
    */
  def unary_- : IntVar = {
    val result    = IntVar(safeNeg(this.max()), safeNeg(this.min()))
    val c         = new XplusYeqC(this, result, 0)
    model.constr += c
    result
  }

  /** Defines an 'equation' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: IntVar): PrimitiveConstraint = {
    val c         = new XeqY(this, that)
    model.constr += c
    c
  }

  /** Defines an 'equation' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: Int): PrimitiveConstraint = {
    val c         = new XeqC(this, that)
    model.constr += c
    c
  }

  /** Defines an 'inequality' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the 'inequality' constraint.
    * @return the defined constraint.
    */
  def #!= (that: IntVar): PrimitiveConstraint = {
    val c         = new XneqY(this, that)
    model.constr += c
    c
  }

  /** Defines an 'inequality' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'inequality' constraint.
    * @return the defined constraint.
    */
  def #!= (that: Int): PrimitiveConstraint = {
    val c         = new XneqC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the 'less than' constraint.
    * @return the defined constraint.
    */
  def #< (that: IntVar): PrimitiveConstraint = {
    val c         = new XltY(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'less than' constraint.
    * @return the equation constraint.
    */
  def #< (that: Int): PrimitiveConstraint = {
    val c         = new XltC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than or equal' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for "less than or equal" constraint.
    * @return the defined constraint.
    */
  def #<= (that: IntVar): PrimitiveConstraint = {
    val c         = new XlteqY(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than or equal' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'less than or equal' constraint.
    * @return the equation constraint.
    */
  def #<= (that: Int): PrimitiveConstraint = {
    val c         = new XlteqC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the 'greater than' constraint.
    * @return the defined constraint.
    */
  def #> (that: IntVar): PrimitiveConstraint = {
    val c         = new XgtY(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'greater than' constraint.
    * @return the equation constraint.
    */
  def #> (that: Int): PrimitiveConstraint = {
    val c         = new XgtC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than or equal' constraint between two `IntVar` instances.
    *
    * @param that a second parameter for the 'greater than or equal' constraint.
    * @return the defined constraint.
    */
  def #>= (that: IntVar): PrimitiveConstraint = {
    val c         = new XgteqY(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than or equal' constraint between this variable and an integer constant.
    *
    * @param that a second parameter for the 'greater than or equal' constraint.
    * @return the equation constraint.
    */
  def #>= (that: Int): PrimitiveConstraint = {
    val c         = new XgteqC(this, that)
    model.constr += c
    c
  }

  /** Defines a constraint on inclusion of an `IntVar` variable value in a set.
    *
    * @param that set that this variable's value must be included in.
    * @return the defined constraint.
    */
  def in(that: SetVar): PrimitiveConstraint = {
    val c = if (min == max)
      new EinA(min, that)
    else
      new XinA(this, that)

    model.constr += c
    c
  }
}