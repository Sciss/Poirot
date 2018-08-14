/*
 *  DoubleVar.scala
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

import org.jacop.floats.constraints._
import org.jacop.{core => jc}
import org.jacop.floats.{core => jfc}

object DoubleVar {
  /** Creates an anonymous finite domain floating point variable.
    *
    * @param min minimal value of variable's domain.
    * @param max maximal value of variable's domain.
    */
  def apply(min: Double, max: Double)(implicit model: Model): DoubleVar = {
    val res = apply ("_$" + model.n, min, max)
    model.n += 1
    res
  }

  /** Creates an anonymous floating point variable.
    * Minimum and maximum are set to defaults `-1e150` and `+1e150`.
    *
    * @param name variable's identifier.
    */
  def apply(name: String)(implicit model: Model): DoubleVar = {
    val res = apply (name, -1e150, 1e150)
    model.n += 1
    res
  }

  /** Creates an anonymous floating point variable.
    * Minimum and maximum are set to defaults `-1e150` and `+1e150`.
    */
  def apply()(implicit model: Model): DoubleVar = {
    val res = apply (-1e150, 1e150)
    model.n += 1
    res
  }

  /** Creates a floating point variable and its primitive constraints.
    *
    * Note that on the JaCoP side, this is represented by `FloatVar`,
    * although the precision is 64-bit.
    *
    * @param name variable identifier.
    * @param min  minimal value of variable's domain.
    * @param max  maximal value of variable's domain.
    */
  def apply(name: String, min: Double, max: Double)(implicit model: Model): DoubleVar =
    new DoubleVar(name, min, max)
}
/** A floating point variable and its primitive constraints.
  */
class DoubleVar protected (name: String, min: Double, max: Double)(implicit model: Model) 
  extends jfc.FloatVar(model, name, min, max) {

  /** Defines an 'add' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'addition' constraint.
    * @return variable being the result of the constraint.
    */
  def + (that: jfc.FloatVar): DoubleVar = {
    val result = DoubleVar()
    val c = new PplusQeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines an 'add' constraint between this variable and an another `Double` value.
    *
    * @param that a second double parameter for the 'addition' constraint.
    * @return variable being the result of the constraint.
    */
  def + (that: Double): DoubleVar = {
    val result = DoubleVar()
    val c = new PplusCeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'subtract' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'subtraction' constraint.
    * @return variable being the result of the constraint.
    */
  def - (that: jfc.FloatVar): DoubleVar = {
    val result = DoubleVar()
    val c = new PminusQeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'subtract' constraint between this variable and a `Double` value.
    *
    * @param that a second `Double` parameter for the 'subtraction' constraint.
    * @return variable being the result of the constraint.
    */
  def - (that: Double): DoubleVar = {
    val result = DoubleVar()
    val c = new PminusCeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'multiplication' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'multiplication' constraint.
    * @return variable being the result of the constraint.
    */
  def * (that: jfc.FloatVar): DoubleVar = {
    val result = DoubleVar()
    val c = new PmulQeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'multiplication' constraint between this variable and a `Double` value.
    *
    * @param that a second parameter for the 'multiplication' constraint.
    * @return variable being the result of the constraint.
    */
  def * (that: Double): DoubleVar = {
    val result = DoubleVar()
    val c = new PmulCeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a 'division' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'division' constraint.
    * @return variable being the result of the constraint.
    */
  def / (that: jfc.FloatVar): DoubleVar = {
    val result = DoubleVar()
    val c = new PdivQeqR(this, that, result)
    model.constr += c
    result
  }

  /** Defines a unary "-" constraint for this variable.
    *
    * @return variable being the result of the unary negation constraint.
    */
  def unary_- : DoubleVar = {
    val result = DoubleVar()
    val c = new PplusQeqR(this, result, DoubleVar(0.0, 0.0))
    model.constr += c
    result
  }


  /** Defines an 'equation' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: jfc.FloatVar): PeqQ = {
    val c = new PeqQ(this, that)
    model.constr += c
    c
  }

  /** Defines an 'equation' constraint between this variable and an `IntVar`.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: jc.IntVar): XeqP = {
    val c = new org.jacop.floats.constraints.XeqP(that, this)
    model.constr += c
    c
  }

  /** Defines an 'equation' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'equation' constraint.
    * @return the defined constraint.
    */
  def #= (that: Double): PeqC = {
    val c = new PeqC(this, that)
    model.constr += c
    c
  }

  /** Defines an 'inequality' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'inequality' constraint.
    * @return the defined constraint.
    */
  def #!= (that: jfc.FloatVar): PneqQ = {
    val c = new PneqQ(this, that)
    model.constr += c
    c
  }

  /** Defines an 'inequality' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'inequality' constraint.
    * @return the defined constraint.
    */
  def #!= (that: Double): PneqC = {
    val c = new PneqC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'less than' constraint.
    * @return the defined constraint.
    */
  def #< (that: jfc.FloatVar): PltQ = {
    val c = new PltQ(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'less than' constraint.
    * @return the defined constraint.
    */
  def #< (that: Double): PltC = {
    val c = new PltC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than or equal' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'less than or equal' constraint.
    * @return the defined constraint.
    */
  def #<= (that: jfc.FloatVar): PlteqQ = {
    val c = new PlteqQ(this, that)
    model.constr += c
    c
  }

  /** Defines a 'less than or equal' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'less than or equal' constraint.
    * @return the defined constraint.
    */
  def #<= (that: Double): PlteqC = {
    val c = new PlteqC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'greater than' constraint.
    * @return the defined constraint.
    */
  def #> (that: jfc.FloatVar): PgtQ = {
    val c = new PgtQ(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'greater than' constraint.
    * @return the defined constraint.
    */
  def #> (that: Double): PgtC = {
    val c = new PgtC(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than or equal' constraint between two `DoubleVar` instances.
    *
    * @param that a second parameter for the 'greater than or equal' constraint.
    * @return the defined constraint.
    */
  def #>= (that: jfc.FloatVar): PgteqQ = {
    val c = new PgteqQ(this, that)
    model.constr += c
    c
  }

  /** Defines a 'greater than or equal' constraint between this variable and a `Double` constant.
    *
    * @param that a second parameter for the 'greater than or equal' constraint.
    * @return the defined constraint.
    */
  def #>= (that: Double): PgteqC = {
    val c = new PgteqC(this, that)
    model.constr += c
    c
  }
}
