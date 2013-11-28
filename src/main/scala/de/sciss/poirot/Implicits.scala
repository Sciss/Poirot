package de.sciss.poirot

import language.implicitConversions
import org.jacop.constraints.{Reified, Constraint, PrimitiveConstraint}
import scala.collection.immutable.{Seq => ISeq, Iterable => IIterable}
import de.sciss.poirot

/** Implicit conversions of Int and Bool to IntVar and BoolVar.
  * Used in overloaded operators.
  */
object Implicits {
  /** Converts integer to IntVar.
    *
    * @param i intger to be converted.
    */
  implicit def intToJacopVar(i: Int)(implicit model: Model): IntVar = IntVar(i, i)

  /** Converts integer to BoolVar.
   *
   * @param b boolean to be converted.
   */
  implicit def booleanToJacopVar(b: Boolean)(implicit model: Model): BooleanVar = {
    val i = if (b) 1 else 0
    new BooleanVar(i, i)
  }


  implicit class Reifier(val peer: PrimitiveConstraint) extends AnyVal {
    def #<-> (b: BooleanVar)(implicit model: Model): Constraint = {
      val c = new Reified(peer, b)
      model.constr.remove(model.constr.length - 1)
      model.constr += c
      c
    }
  }

  implicit class PoirotIntVarIterable(val peer: IIterable[IntVar]) extends AnyVal {
    def allDifferent()(implicit model: Model): Unit = poirot.allDifferent(peer)
    def allDistinct ()(implicit model: Model): Unit = poirot.allDistinct (peer)

    // def sum(implicit model: Model): IntVar = poirot.sum(peer)
  }

  implicit class PoirotIntVarSeq(val peer: ISeq[IntVar]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): IntVar = poirot.intVarAt(index, peer)
  }

  implicit class PoirotIntSeq(val peer: ISeq[Int]) extends AnyVal {
    def apply(index: IntVar)(implicit model: Model): IntVar = poirot.intAt(index, peer)
  }
}