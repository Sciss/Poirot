package de.sciss.poirot

import language.implicitConversions

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

  //  /** Converts Array to List, if needed.
  //    *
  //    * @param a array to be converted.
  //    */
  //  implicit def arrayToList[A](a: Array[A]) = a.toList
}