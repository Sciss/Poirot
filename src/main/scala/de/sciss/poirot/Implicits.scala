/*
 *  Implicits.scala
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

import de.sciss.poirot
import org.jacop.constraints.{Constraint, PrimitiveConstraint, Reified}

import scala.collection.immutable.{Iterable => IIterable, Seq => ISeq}
import scala.language.implicitConversions

/** Implicit conversions of Int and Bool to IntVar and BoolVar.
  * Used in overloaded operators.
  */
object Implicits {
  /** Converts an integer value to an `IntVar`.
    *
    * @param i integer to be converted.
    */
  implicit def intToJacopVar(i: Int)(implicit model: Model): IntVar = IntVar(i, i)

  /** Converts a boolean value to a `BooleanVar`.
   *
   * @param b boolean to be converted.
   */
  implicit def booleanToJacopVar(b: Boolean)(implicit model: Model): BooleanVar = {
    val i = if (b) 1 else 0
    new BooleanVar(i, i)
  }

  /** Converts a double value to a `DoubleVar`.
    *
    * @param d double to be converted.
    */
  implicit def doubleToJacopVar(d: Double)(implicit model: Model): DoubleVar = DoubleVar(d, d)

  implicit class Reifier(val peer: PrimitiveConstraint) extends AnyVal {
    def #<-> (b: BooleanVar)(implicit model: Model): Constraint = {
      val c = new Reified(peer, b)
      model.constr.remove(model.constr.length - 1)
      model.constr += c
      c
    }
  }

  implicit class PoirotIntVarIterable(val peer: IIterable[IntVar]) extends AnyVal {
    def allDifferent()(implicit model: Model): Unit = poirot.allDifferent(peer.toSeq: _*)
    def allDistinct ()(implicit model: Model): Unit = poirot.allDistinct (peer.toSeq: _*)

    // def sum(implicit model: Model): IntVar = poirot.sum(peer)
  }

  implicit class PoirotIntVarSeq(val peer: ISeq[IntVar]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): IntVar = poirot.intVarAt(index, peer)
  }

  implicit class PoirotIntSeq(val peer: ISeq[Int]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): IntVar = poirot.intAt(index, peer)
  }

  implicit class PoirotBooleanVarSeq(val peer: ISeq[BooleanVar]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): BooleanVar = poirot.booleanVarAt(index, peer)
  }

  implicit class PoirotDoubleSeq(val peer: ISeq[Double]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): DoubleVar = poirot.doubleAt(index, peer)
  }
}