//package de.sciss.poirot
//
//import org.jacop.{core => jc}
//import org.jacop.{constraints => cons}
//
//import scala.collection.immutable.{Iterable => IIterable}
//
//final class SumInt(private val xs: IIterable[IntVar]) extends AnyVal {
//  def #=  (result: IntVar)(implicit model: Model): cons.SumInt = make("==", result)
//  def #!= (result: IntVar)(implicit model: Model): cons.SumInt = make("!=", result)
//  def #<  (result: IntVar)(implicit model: Model): cons.SumInt = make("<" , result)
//  def #>  (result: IntVar)(implicit model: Model): cons.SumInt = make(">" , result)
//  def #<= (result: IntVar)(implicit model: Model): cons.SumInt = make("<=", result)
//  def #>= (result: IntVar)(implicit model: Model): cons.SumInt = make(">=", result)
//
//  private def make(rel: String, result: IntVar)(implicit model: Model): cons.SumInt = {
//    val c = new cons.SumInt(xs.toArray[jc.IntVar], rel, result)
//    if (trace) println(c)
//    model.constr += c
//    c
//  }
//}
