/*
 *  package.scala
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

package de.sciss

import org.jacop.constraints.binpacking.Binpacking
import org.jacop.constraints.knapsack.Knapsack
import org.jacop.constraints.netflow.NetworkFlow
import org.jacop.constraints.regular.Regular
import org.jacop.constraints.{netflow => jnet, _}
import org.jacop.floats.constraints.{AbsPeqR, AcosPeqR, AsinPeqR, AtanPeqR, CosPeqR, ElementFloat, ExpPeqR, LinearFloat, LnPeqR, SinPeqR, SqrtPeqR, TanPeqR}
import org.jacop.floats.search.SplitSelectFloat
import org.jacop.search._
import org.jacop.set.constraints.{CardA, CardAeqX, Match}
import org.jacop.set.search._
import org.jacop.set.{core => jset}
import org.jacop.{core => jc}
import org.jacop.floats.{core => jfc}

import scala.collection.immutable.{Iterable => IIterable, Seq => ISeq}
import scala.collection.{breakOut, mutable}
import scala.reflect.ClassTag

/** Package for defining variables, constraints, global constraints and search
  * methods for JaCoP constraint solver in Scala.
  */
package object poirot {
  type Vec[+A]  = collection.immutable.IndexedSeq[A]
  val  Vec      = collection.immutable.IndexedSeq

  var trace = false

  private val addLabelFun = new ThreadLocal[mutable.Buffer[DepthFirstSearch[_ <: jc.Var]]]

  private def addLabel(label: DepthFirstSearch[_ <: jc.Var]): Unit = {
    val b = addLabelFun.get()
    if (b != null) b += label
  }

  /** The maximum number of solutions to be explored.
    * `-1` indicates that there is no limit. */
  var maxNumSolutions: Int = -1

  /** The search time out in seconds. `-1` indicates that there is no time out. */
  var timeOut: Int = -1

  var recordSolutions = false

  // =============== Global constraints ===============

  /** Wrapper for [[org.jacop.constraints.Alldiff]].
    *
    * @param xs set of variables to be different.
    */
  def allDifferent(xs: IntVar*)(implicit model: Model): Unit = {
    val c = new Alldiff(xs.toArray[jc.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.Alldistinct]].
    *
    * @param xs set of variables to be different.
    */
  def allDistinct(xs: IntVar*)(implicit model: Model): Unit = {
    val c = new Alldistinct(xs.toArray[jc.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.GCC]] (global cardinality constraint).
    *
    * @param xs array of tuples of variables and their counters
    */
  def gcc(xs: IIterable[(IntVar, IntVar)])(implicit model: Model): Unit = {
    val (vars, counters) = xs.unzip
    val c = new GCC(vars.toArray[jc.IntVar], counters.toArray[jc.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

//  /** First stage for producing [[org.jacop.constraints.SumInt]].
//    */
//  def sum(res: IIterable[IntVar]): poirot.SumInt = new poirot.SumInt(res)

  def sum(res: IIterable[IntVar])(implicit model: Model): IntVar = {
    val result = IntVar()
    val c = new SumInt(res.toArray[jc.IntVar], "==", result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[org.jacop.constraints.SumWeight]].
  //    *
  //    * @param xs tuples consisting of variables and their corresponding weights
  //    * @param sum summation result.
  //    */
  //  def assignWeightedSum(xs: IIterable[(IntVar, Int)], sum: IntVar)(implicit model: Model): Unit = {
  //    val (vars, weights) = xs.unzip
  //    val c = new SumWeight(vars.toArray[jc.IntVar], weights.toArray[Int], sum)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.constraints.SumWeight]].
    *
    * @param tup      tuples of variables to sum and their weights
    * @return         summation result.
    */
  def weightedSum(tup: IIterable[(IntVar, Int)])(implicit model: Model): IntVar = {
    val result          = IntVar()
    val (vars, weights) = tup.unzip
    val c               = new SumWeight(vars.toArray[jc.IntVar], weights.toArray[Int], result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.constraints.AbsXeqY]].
    *
    * @param x variable for abs operation.
    * @return absolute value result.
    */
  def abs(x: IntVar)(implicit model: Model): IntVar = {
    val result  = IntVar()
    val c       = new AbsXeqY(x, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  //  /** Wrapper for [[org.jacop.constraints.Max]].
  //    *
  //    * @param xs variables where maximum values is to be found.
  //    * @param mx maximum value.
  //    */
  //  def assignMax(xs: IIterable[IntVar], mx: jc.IntVar)(implicit model: Model): Unit = {
  //    val c = new Max(xs.toArray[jc.IntVar], mx)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  //  /** Wrapper for [[org.jacop.constraints.Min]].
  //    *
  //    * @param xs array of variables where minimum values is to be found.
  //    * @param mn minimum value.
  //    */
  //  def assignMin(xs: IIterable[IntVar], mn: jc.IntVar)(implicit model: Model): Unit = {
  //    val c = new Min(xs.toArray[jc.IntVar], mn)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.constraints.Max]].
    *
    * @param xs variables where maximum values is to be found.
    * @return max value.
    */
  def max(xs: IntVar*)(implicit model: Model): IntVar = {
    val result    = IntVar()
    val c         = new Max(xs.toArray[jc.IntVar], result)
    model.constr += c
    result
  }

  /** Wrapper for [[org.jacop.constraints.Min]].
    *
    * @param xs variables where minimum values is to be found.
    * @return minimum value.
    */
  def min(xs: IntVar*)(implicit model: Model): IntVar = {
    val result    = IntVar()
    val c         = new Min(xs.toArray[jc.IntVar], result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[org.jacop.constraints.Count]].
  //    *
  //    * @param xs variables to count number of values value.
  //    * @param count of values value.
  //    */
  //  def assignCount(xs: IIterable[IntVar], value: Int, count: IntVar)(implicit model: Model): Unit = {
  //    val c = new Count(xs.toArray[jc.IntVar], count, value)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.constraints.Count]].
    *
    * @param xs variables to count number of values value.
    * @return number of values value.
    */
  def count(xs: IIterable[IntVar], value: Int)(implicit model: Model): IntVar = {
    val result    = IntVar()
    val c         = new Count(xs.toArray[jc.IntVar], result, value)
    model.constr += c
    result
  }

  //  /** Wrapper for [[org.jacop.constraints.Values]].
  //    *
  //    * @param xs variables to count number of different values.
  //    * @param count of different values.
  //    */
  //  def assignNumDistinct(xs: IIterable[IntVar], count: IntVar)(implicit model: Model): Unit = {
  //    val c = new Values(xs.toArray[jc.IntVar], count)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.constraints.Values]].
    *
    * @param xs variables to count number of different values.
    * @return number of different values.
    */
  def numDistinct(xs: IIterable[IntVar])(implicit model: Model): IntVar = {
    val result    = IntVar()
    val c         = new Values(xs.toArray[jc.IntVar], result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[org.jacop.constraints.Element]].
  //    *
  //    * @param index    index to select element from list of elements.
  //    * @param xs       sequence of integers that can be assigned to values.
  //    * @param value    value selected from list of elements.
  //    */
  //  def assignElementAt(index: jc.IntVar, xs: Vec[Int], value: jc.IntVar)(implicit model: Model): Unit = {
  //    val c = new Element(index, xs.toArray, value)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  //  /** Wrapper for [[org.jacop.constraints.Element]].
  //    *
  //    * @param index    index to select element from list of elements.
  //    * @param xs       sequence of integers that can be assigned to values.
  //    * @param value    value selected from list of elements.
  //    * @param offset   value of index offset (shift).
  //    */
  //  def assignElementAt(index: jc.IntVar, xs: Vec[Int], value: jc.IntVar, offset: Int = 0)
  //             (implicit model: Model): Unit = {
  //    val c = new Element(index, xs.toArray, value, offset)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.constraints.Element]].
    *
    * @param index    index to select element from list of elements.
    * @param xs       sequence of integers that can be assigned to values.
    * @param offset   value of index offset (shift).
    * @return         the variable yielding the element at the given index
    */
  def intAt(index: IntVar, xs: ISeq[Int], offset: Int = 0)(implicit model: Model): IntVar = {
    val result  = IntVar()
    val c       = new Element(index, xs.toArray, result, offset)
    if (trace) println(c)
    model.impose(c)
    result
  }

  //  def elementAt_=(index: IntVar, xs: Vec[Int], result: IntVar)(implicit model: Model): Unit = {
  //    val c       = new Element(index, xs.toArray, result)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  def intVarAt(index: IntVar, xs: ISeq[IntVar], offset: Int = 0)(implicit model: Model): IntVar = {
    val result  = IntVar()
    val c       = new Element(index, xs.toArray[jc.IntVar], result, offset)
    if (trace) println(c)
    model.impose(c)
    result
  }

  def booleanVarAt(index: IntVar, xs: ISeq[IntVar], offset: Int = 0)(implicit model: Model): BooleanVar = {
    val result  = BooleanVar()
    val c       = Element.choose(index, xs.toArray[jc.IntVar], result, offset)
    if (trace) println(c)
    model.impose(c)
    result
  }

  def doubleAt(index: IntVar, xs: ISeq[Double], offset: Int = 0)(implicit model: Model): DoubleVar = {
    val result  = DoubleVar()
    val c       = new ElementFloat(index, xs.toArray, result, offset)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.constraints.Diff2]].
    *
    * XXX TODO: remove arrays, unify sequences
    *
    * @param x coordinate X of rectangle.
    * @param y coordinate Y of rectangle.
    * @param lx length in derection X of rectangle.
    * @param ly length in derection Y of rectangle.
    */
  def diff2(x: Array[IntVar], y: Array[IntVar], lx: Array[IntVar], ly: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Diff(x.asInstanceOf[Array[jc.IntVar]], y.asInstanceOf[Array[jc.IntVar]],
      lx.asInstanceOf[Array[jc.IntVar]], ly.asInstanceOf[Array[jc.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.Diff2]].
    *
    * @param rectangles sequence of four element vectors representing rectangles [x, y, lx, ly]
    */
  def diff2(rectangles: Vec[Vec[IntVar]])(implicit model: Model): Unit = {
    val arr: Array[Array[jc.IntVar]] = rectangles.map(_.toArray[jc.IntVar])(breakOut)
    val c = new Diff(arr)
    if (trace) println(c)
    model.impose(c) // new Diff(rectangles.asInstanceOf[Array[Array[jc.IntVar]]]))
  }

  /** Wrapper for [[org.jacop.constraints.Cumulative]].
    *
    * @param xs     tuples consisting of (t, d, r), where t is start times of tasks, d is duration of tasks,
    *               r is number of resources of tasks.
    * @param limit  limit on number of resources used in a schedule.
    */
  def cumulative(xs: IIterable[(IntVar, IntVar, IntVar)], limit: IntVar)(implicit model: Model): Unit = {
    val (t, d, r) = xs.unzip3
    val c         = new Cumulative(t.toArray[jc.IntVar], d.toArray[jc.IntVar], r.toArray[jc.IntVar], limit)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.Circuit]].
    *
    * @param nodes variables, which domains define next nodes in the graph.
    */
  def circuit(nodes: IIterable[IntVar])(implicit model: Model): Unit = {
    val c = new Circuit(nodes.toArray[jc.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.Assignment]].
    *
    * XXX TODO: remove arrays, unify sequences
    *
    * @param x array of variables.
    * @param y array variables that values are permutation of x.
    */
  def assignment(x: Array[IntVar], y: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Assignment(x.asInstanceOf[Array[jc.IntVar]], y.asInstanceOf[Array[jc.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.Among]].
    *
    * XXX TODO: rename to `assignAmong`, add proper `among`
    *
    * @param xs   variables.
    * @param kSet values to be checked.
    * @param n    number of values found.
    */
  def among(xs: IIterable[IntVar], kSet: IntSet, n: IntVar)(implicit model: Model): Unit = {
    val c = new Among(xs.toArray[jc.IntVar], kSet, n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.AmongVar]].
    *
    * XXX TODO: remove arrays, unify sequences, rename to `assignAmong`, add proper `among`
    *
    * @param listX array of variables.
    * @param listY array of variables to be checked if their values .
    * @param n number of values found.
    */
  def among(listX: Array[IntVar], listY: Array[IntVar], n: IntVar)(implicit model: Model): Unit = {
    val c = new AmongVar(listX.asInstanceOf[Array[jc.IntVar]], listY.asInstanceOf[Array[jc.IntVar]], n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.ExtensionalSupportVA]].
    *
    * XXX TODO: rename to `assignTable`, add proper `table`
    *
    * @param list   sequence of tuples consisting of variables and sequences of allowed values be assigned
    */
  def table(list: IIterable[(IntVar, IIterable[Int])])(implicit model: Model): Unit = {
    val (xs, tup) = list.unzip
    val arr: Array[Array[Int]] = tup.map(_.toArray)(breakOut)
    val c = new ExtensionalSupportVA(xs.toArray[jc.IntVar], arr)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.knapsack.Knapsack]].
    *
    * XXX TODO: replace arrays, unify tuples
    *
    * @param profits array of profite for items.
    * @param weights array of weights for items.
    * @param quantity array of quantities of items.
    * @param knapsackCapacity knapsack capacity.
    * @param knapsackProfit profite when selling items.
    */
  def knapsack(profits: Array[Int], weights: Array[Int], quantity: List[IntVar],
               knapsackCapacity: IntVar, knapsackProfit: IntVar)(implicit model: Model): Unit = {
    val c = new Knapsack(profits, weights, quantity.toArray, knapsackCapacity, knapsackProfit)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for org.jacop.constraints.binpack.Binpack (?).
    *
    * @param bins list containing which tuples of bins, their loads and their weights
    */
  def binPacking(bins: (IntVar, IntVar, Int)*)(implicit model: Model): Unit = {
    val (b, load, w) = bins.unzip3
    val c = new Binpacking(b.toArray[jc.IntVar], load.toArray[jc.IntVar], w.toArray)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[org.jacop.constraints.regular.Regular]].
    *
    * @param dfa  specification of finite state machine using class fsm.
    * @param vars list of variables assigned to fsm nodes.
    */
  def regular(dfa: FSM, vars: IIterable[IntVar])(implicit model: Model): Unit = {
    val c = new Regular(dfa, vars.toArray)
    if (trace) println(c)
    model.impose(c)
  }

  // ================== Decompose constraints

  /** Wrapper for [[org.jacop.constraints.Sequence]].
    *
    * @param list list of variables to be constrained.
    * @param set set of values to be checked.
    * @param q length of the sub-sequence.
    * @param min minimal number of occurrences of values in the sub-sequence.
    * @param max maximal number of occurrences of values in the sub-sequence.
    */
  def sequence(list: Array[IntVar], set: IntSet, q: Int, min: Int, max: Int)(implicit model: Model): Unit = {
    val c = new Sequence(list.asInstanceOf[Array[jc.IntVar]], set, q, min, max)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[org.jacop.constraints.Stretch]].
    *
    * @param values a list of values to be assigned to sub-sequences.
    * @param min minimal length of the sub-sequence for each value on position i.
    * @param max maximal length of the sub-sequence for each value on position i.
    * @param x list of variables to be constrained.
    */
  def stretch(values: Array[Int], min: Array[Int], max: Array[Int], x: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Stretch(values, min, max, x.asInstanceOf[Array[jc.IntVar]])
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[org.jacop.constraints.Lex]].
    *
    * @param x array of vectors of variables to be lexicographically ordered.
    */
  def lex(x: Array[Array[IntVar]])(implicit model: Model): Unit = {
    val c = new org.jacop.constraints.Lex(x.asInstanceOf[Array[Array[jc.IntVar]]])
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[org.jacop.constraints.SoftAlldifferent]].
    *
    * @param xVars    array of variables to be constrained to be different.
    * @param costVar  measures degree of violation (uses value based violation).
    */
  def softAllDifferent(xVars: Array[IntVar], costVar: IntVar)(implicit model: Model): Unit = {
    val violationMeasure = ViolationMeasure.VALUE_BASED
    val c = new SoftAlldifferent(xVars.asInstanceOf[Array[jc.IntVar]], costVar, violationMeasure)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[org.jacop.constraints.SoftGCC]].
    *
    * @param xVars array of variables to be constrained to be different.
    * @param hardLowerBound  lower bound on limits that can not be violated.
    * @param hardUpperBound  upper bound on limits that can not be violated
    * @param countedValue values that are counted.
    * @param softCounters specifies preferred values for counters and can be violated.
    */
  def softGCC(xVars: Array[IntVar], hardLowerBound: Array[Int], hardUpperBound: Array[Int],
              countedValue: Array[Int], softCounters: Array[IntVar], costVar: IntVar)
             (implicit model: Model): Unit = {
    val violationMeasure = ViolationMeasure.VALUE_BASED
    val c = new SoftGCC(xVars.asInstanceOf[Array[jc.IntVar]],
      hardLowerBound,
      hardUpperBound,
      countedValue,
      softCounters.asInstanceOf[Array[jc.IntVar]],
      costVar, violationMeasure)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  def networkFlow(net: jnet.NetworkBuilder)(implicit model: Model): Unit = {
    val c = new NetworkFlow(net)
    if (trace) println(c)
    model.impose(c)
  }

  // ================== Logical operations on constraints


  /** Wrapper for [[org.jacop.constraints.Or]].
    *
    * @param list constraints to be disjunction.
    * @return the constraint that is a a disjunction of constraints.
    */
  def OR(list: PrimitiveConstraint*)(implicit model: Model): PrimitiveConstraint = {
    val c = new Or(list.toArray)
    list.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[org.jacop.constraints.And]].
    *
    * @param xs constraints to be conjunction.
    * @return the constraint that is a a conjunction of constraints.
    */
  def AND(xs: PrimitiveConstraint*)(implicit model: Model): PrimitiveConstraint = {
    val c = new And(xs.toArray)
    xs.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[org.jacop.constraints.Not]].
    *
    * @param constr constraints to be negated.
    * @return the negated constraint.
    */
  def NOT(constr: PrimitiveConstraint)(implicit model: Model): PrimitiveConstraint = {
    val c = new Not(constr)
    model.constr.remove(model.constr.indexOf(constr))
    model.constr += c
    c
  }

  // =============== Set constraints ===============


  /** Wrapper for [[org.jacop.set.constraints.CardAeqX]].
    *
    * @param s constrained set variable.
    * @return variable defining cardinality of s.
    */
  def card(s: SetVar)(implicit model: Model): IntVar = {
    val result    = IntVar()
    val c         = new CardAeqX(s, result)
    model.constr += c
    result
  }

  /** Wrapper for [[org.jacop.set.constraints.CardA]].
    *
    * @param s constrained set variable.
    * @param n cardinality.
    */
  def card(s: SetVar, n: Int)(implicit model: Model): Unit = {
    val c = new CardA(s, n)
    if (trace) println(c)
    model.impose(c)
  }

  //  /** Wrapper for [[org.jacop.set.constraints.CardAeqX]].
  //    *
  //    * @param s constrained set variable.
  //    * @param n cardinality (IntVar variable).
  //    */
  //  def assignCard(s: SetVar, n: jc.IntVar)(implicit model: Model): Unit = {
  //    val c = new CardAeqX(s, n)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[org.jacop.set.constraints.Match]].
    *
    * @param a    a set variable to be matched against list of IntVar.
    * @param list variables that get values from the set.
    */
  def matching[A <: jc.IntVar](a: SetVar, list: IIterable[A])(implicit model: Model): Unit = {
    val c = new Match(a, list.toArray)
    if (trace) println(c)
    model.impose(c)
  }


  // =============== Floating point constraints ===================

  /** Wrapper for [[org.jacop.floats.constraints.AbsPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return absolute value of the variable.
    */
  def abs(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new AbsPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.ExpPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of exponential function the variable.
    */
  def exp(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new ExpPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.LnPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of natural logarithm function the variable.
    */
  def ln(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new LnPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.SqrtPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of square root function the variable.
    */
  def sqrt(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new SqrtPeqR(a, result)
    if (trace) println(c)
    model.impose( c )
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.SinPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of sinus function the variable.
    */
  def sin(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new SinPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.AsinPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of arc-sine function the variable.
    */
  def asin(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new AsinPeqR(a, result)
    if (trace) println(c)
    model.impose( c )
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.CosPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of cosine function the variable.
    */
  def cos(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new CosPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.AcosPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of arc-cosine function the variable.
    */
  def acos(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new AcosPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.TanPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of tangent function the variable.
    */
  def tan(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new TanPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.AtanPeqR]].
    *
    * @param a a `DoubleVar` variable.
    * @return value of arc-tangent function the variable.
    */
  def atan(a: DoubleVar)(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val c = new AtanPeqR(a, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.LinearFloat]].
    *
    * @param res array of variables to be summed up. 
    * @return summation result. 
    */
  def sum(res: IIterable[DoubleVar])(implicit model: Model): DoubleVar = {
    val result  = DoubleVar()
    val vecB    = Array.newBuilder[jfc.FloatVar]
    res.foreach { in =>
      vecB += in
    }
    vecB += result
    val vec = vecB.result()
    val w = Array.fill(vec.length)(1.0)
    w(w.length - 1) = -1.0

    val c = new LinearFloat(vec, w, "==", 0.0)
    if (trace) println(c)
    model.constr += c
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.LinearFloat]].
    *
    * @param res array of variables to be summed up. 
    * @return summation result. 
    */
  def weightedSum(res: IIterable[DoubleVar], weight: IIterable[Double])(implicit model: Model): DoubleVar = {
    val result = DoubleVar()
    val vecB    = Array.newBuilder[jfc.FloatVar]
    res.foreach { in =>
      vecB += in
    }
    vecB += result
    val vec = vecB.result()
    val wB = Array.newBuilder[Double]
    weight.foreach { in =>
      wB += in
    }
    wB += -1.0
    val w = wB.result()
    val c = new LinearFloat(vec, w, "==", 0.0)
    if (trace) println(c)
    model.constr += c
    result
  }

  /** Wrapper for [[org.jacop.floats.constraints.LinearFloat]].
    *
    * @param res array of variables to be summed up. 
    * @return summation result. 
    */
  def linear(res: IIterable[DoubleVar], weight: IIterable[Double], result: Double)(implicit model: Model): Unit = {
    val resA  = res.toArray[jfc.FloatVar]
    val wA    = weight.toArray
    val c     = new LinearFloat(resA, wA, "==", result)
    if (trace) println(c)
    model.constr += c
  }

  // =============== Search methods ===================

  /** Minimization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def minimize[A <: jc.Var](select: SelectChoicePoint[A], cost: IntVar, printSolutions: (() => Unit)*)
                                   (implicit m: ClassTag[A], model: Model): Boolean =
    minimizeImpl(select, cost, printSolutions)

  /** Minimization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def minimize[A <: jc.Var](select: SelectChoicePoint[A], cost: DoubleVar, printSolutions: (() => Unit)*)
                           (implicit m: ClassTag[A], model: Model): Boolean =
    minimizeImpl(select, cost, printSolutions)

  private def minimizeImpl[A <: jc.Var](select: SelectChoicePoint[A], cost: jc.Var, printSolutions: Seq[() => Unit])
                                       (implicit m: ClassTag[A], model: Model): Boolean = {
    model.imposeAllConstraints()

    val label = dfs[A](all = false)
    addLabel(label)

    if (printSolutions.nonEmpty) {
      // label.setSolutionListener(new EmptyListener[A])
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A](printSolutions))
    }

    if (timeOut > 0) label.setTimeOut(timeOut)

    if (maxNumSolutions > 0) {
      label.getSolutionListener.setSolutionLimit(maxNumSolutions)
      label.respectSolutionListenerAdvice = true
    }

    label.labeling(model, select, cost)
  }


  /** Maximization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def maximize[A <: jc.Var](select: SelectChoicePoint[A], cost: IntVar,
                                    printSolutions: (() => Unit)*)(implicit m: ClassTag[A], model: Model): Boolean = {
    val costN  = IntVar("newCost")
    costN     #= -cost
    minimize(select, costN, printSolutions: _*)
  }

  /** Maximization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def maximize[A <: jc.Var](select: SelectChoicePoint[A], cost: DoubleVar,
                            printSolutions: (() => Unit)*)(implicit m: ClassTag[A], model: Model): Boolean = {
    val costN  = DoubleVar("newCost")
    costN     #= -cost
    minimize(select, costN, printSolutions: _*)
  }

  /** Search method that finds a solution.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @return true if solution found and false otherwise.
    */
  def satisfy[A <: jc.Var](select: SelectChoicePoint[A], printSolutions: (() => Unit)*)
                                  (implicit m: ClassTag[A], model: Model): Boolean =
    satisfyImpl(select, printSolutions, all = false)

  private def satisfyImpl[A <: jc.Var](select: SelectChoicePoint[A], printSolutions: Seq[() => Unit],
                                               all: Boolean)
                                              (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val label = dfs[A](all = all)
    addLabel(label)

    if (printSolutions.nonEmpty) {
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A](printSolutions))
    }

    val lbList = label.getSolutionListener

    if (timeOut > 0)          label.setTimeOut(timeOut)
    if (all)                  lbList.searchAll(true)
    if (maxNumSolutions > 0)  lbList.setSolutionLimit(maxNumSolutions)
    if (recordSolutions)      lbList.recordSolutions(recordSolutions)

    label.labeling(model, select)
  }

  /** Search method that finds all solutions.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @return true if solution found and false otherwise.
    */
  def satisfyAll[A <: jc.Var](select: SelectChoicePoint[A], printSolutions: (() => Unit)*)
                                     (implicit m: ClassTag[A], model: Model): Boolean =
    satisfyImpl(select, printSolutions, all = true)

  /** Minimization method for sequence of search methods (specified by list of select methods).
    *
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def minimizeSeq[A <: jc.Var](select: IIterable[SelectChoicePoint[A]], cost: IntVar,
                                       printSolutions: (() => Unit)*)
                                      (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val masterLabel = dfs[A](all = false)

    if (printSolutions.nonEmpty) {
      // masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (maxNumSolutions > 0) masterLabel.respectSolutionListenerAdvice = true
    if (timeOut         > 0) masterLabel.setTimeOut(timeOut)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs[A](all = false)
      previousSearch.addChildSearch(label)
      label.setSelectChoicePoint(sel)

      if (printSolutions.nonEmpty) {
        // label.setSolutionListener(new EmptyListener[A])
        label.setPrintInfo(false)
      }

      if (maxNumSolutions > 0) label.respectSolutionListenerAdvice = true
      if (timeOut         > 0) label.setTimeOut(timeOut)

      label
    }

    if (printSolutions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A](printSolutions))

      if (maxNumSolutions > 0) {
        lastLabel.getSolutionListener.setSolutionLimit(maxNumSolutions)
        lastLabel.respectSolutionListenerAdvice = true
      }
    }

    masterLabel.labeling(model, select.head, cost)
  }

  /** Maximization method for sequence of search methods (specified by list of select methods).
    *  
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def maximizeSeq[A <: jc.Var](select: IIterable[SelectChoicePoint[A]], cost: IntVar,
                                       printSolutions: (() => Unit)*)
                                      (implicit m: ClassTag[A], model: Model): Boolean = {

    val costN  = IntVar("newCost", jc.IntDomain.MinInt, jc.IntDomain.MaxInt)
    costN     #= -cost

    minimizeSeq(select, costN, printSolutions: _*)
  }

  /** Search method for finding a solution using a sequence of search methods (specified by list of select methods).
    *
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @return true if solution found and false otherwise.
    */
  def satisfySeq[A <: jc.Var](select: IIterable[SelectChoicePoint[A]], printSolutions: (() => Unit)*)
                                     (implicit m: ClassTag[A], model: Model): Boolean =
    satisfySeqImpl(select, printSolutions, all = false)

  private def satisfySeqImpl[A <: jc.Var](select: IIterable[SelectChoicePoint[A]],
                                                  printSolutions: Seq[() => Unit], all: Boolean)
                                                 (implicit m: ClassTag[A], model: Model): Boolean = {
    model.imposeAllConstraints()

    val masterLabel = dfs[A](all = all)

    if (printSolutions.nonEmpty) {
      // masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (timeOut > 0 ) masterLabel.setTimeOut(timeOut)
    if (all)          masterLabel.getSolutionListener.searchAll(true)

    masterLabel.getSolutionListener.recordSolutions(recordSolutions)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs[A](all = all)
      previousSearch.addChildSearch(label)
      label.setSelectChoicePoint(sel)

      if (printSolutions.nonEmpty) {
        // label.setSolutionListener(new EmptyListener[A])
        label.setPrintInfo(false)
      }

      val lbList = label.getSolutionListener
      if (timeOut > 0 ) label.setTimeOut(timeOut)
      if (all)          lbList.searchAll(true)

      lbList.recordSolutions(recordSolutions)

      label
    }

    if (printSolutions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A](printSolutions))

      if (maxNumSolutions > 0)
        lastLabel.getSolutionListener.setSolutionLimit(maxNumSolutions)
    }

    lastLabel.getSolutionListener.recordSolutions(recordSolutions)

    masterLabel.labeling(model, select.head)
  }

  /** Search method for finding all solutions using a sequence of search methods (specified by list of select methods).
    *  
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @return true if solution found and false otherwise.
    */
  def satisfyAllSeq[A <: jc.Var](select: IIterable[SelectChoicePoint[A]], printSolutions: (() => Unit)*)
                                        (implicit m: ClassTag[A], model: Model): Boolean =
    satisfySeqImpl(select, printSolutions, all = true)

  /** Depth first search method.
    *
    * @return standard depth first search.
    */
  def dfs[A <: jc.Var](all: Boolean): DepthFirstSearch[A] = {
    val label = new DepthFirstSearch[A]

    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[A]())
    if (all)
      label.getSolutionListener.searchAll(true)

    label
  }

  /** Defines list of variables, their selection method for search and value selection
    *  
    * @return select method for search.
    */
  def search[A <: jc.Var](vars: IIterable[A], heuristic: ComparatorVariable[A], indom: Indomain[A])
                         (implicit m: ClassTag[A]): SelectChoicePoint[A] =
    new SimpleSelect[A](vars.toArray, heuristic, indom)

  /** Defines list of variables, their selection method for sequential search and value selection
    *
    * @return select method for search.
    */
  def searchVector[A <: jc.Var](vars: Vec[Vec[A]], heuristic: ComparatorVariable[A],
                                        indom: Indomain[A])(implicit m: ClassTag[A]): SelectChoicePoint[A] = {
    val varsArray: Array[Array[A]] = vars.map(_.toArray)(breakOut)

    new SimpleMatrixSelect[A](varsArray, heuristic, indom)
  }

  /** Defines list of variables, their selection method for split search and value selection
    *
    * @return select method for search.
    */
  def searchSplit[A <: jc.IntVar](vars: IIterable[A], heuristic: ComparatorVariable[A])
                                 (implicit m: ClassTag[A]): SelectChoicePoint[A] =
    new SplitSelect[A](vars.toArray, heuristic, new IndomainMiddle[A]())

  /** Defines list of variables, their selection method for split search and value selection
    *
    * @return select method for search.
    */
  def searchDouble[T <: jfc.FloatVar](vars: IIterable[T], heuristic: ComparatorVariable[T])
                                     (implicit model: Model, m: ClassTag[T]): SelectChoicePoint[T] = {
    new SplitSelectFloat[T](model, vars.toArray, heuristic)
  }

  def withStatistics[Z](block: => Z): (Z, Stats) = {
    val old = addLabelFun.get()
    val b   = mutable.Buffer.empty[DepthFirstSearch[_ <: jc.Var]]
    addLabelFun.set(b)
    try {
      var nodes       = 0
      var decisions   = 0
      var wrong       = 0
      var backtracks  = 0
      var depth       = 0
      var solutions   = 0

      val res = block

      b.foreach { label =>
        nodes       += label.getNodes
        decisions   += label.getDecisions
        wrong       += label.getWrongDecisions
        backtracks  += label.getBacktracks
        depth       += label.getMaximumDepth
        solutions    = label.getSolutionListener.solutionsNo()
      }
      val stats = Stats(nodes = nodes, decisions = decisions, wrong = wrong, backtracks = backtracks, depth = depth,
        solutions = solutions)

      (res, stats)

    } finally {
      addLabelFun.set(old)
    }
  }

  //  def printStatistics(): Unit = {
  //    println()
  //    println(statistics())
  //  }

  /** Defines null variable selection method that is interpreted by JaCoP as input order.
    *
    * XXX TODO: ugly `null`; ought to use place holde value
    *
    * @return related variable selection method.
    */
  def inputOrder[A <: jc.Var]: ComparatorVariable[A] = null

  // ---- Generic (IntVar and BooleanVar) ----

  /** Wrapper for [[org.jacop.search.SmallestDomain]].
    *
    * @return related variable selection method.
    */
  def firstFail[A <: jc.Var] = new SmallestDomain[A]

  /** Wrapper for [[org.jacop.search.MostConstrainedStatic]].
    *
    * @return related variable selection method.
    */
  def mostConstrained[A <: jc.Var] = new MostConstrainedStatic[A]

  /** Wrapper for [[org.jacop.search.LargestDomain]].
    *
    * @return related variable selection method.
    */
  def antiFirstFail[A <: jc.Var] = new LargestDomain[A]

  // ---- IntVar specific ----

  /** Wrapper for [[org.jacop.search.SmallestMin]].
    *
    * @return related variable selection method.
    */
  def smallestMin[A <: jc.IntVar] = new SmallestMin[A]

  /** Wrapper for [[org.jacop.search.SmallestMin]].
    *
    * @return related variable selection method.
    */
  def smallest[A <: jc.IntVar] = new SmallestMin[A]

  /** Wrapper for [[org.jacop.search.LargestMax]].
    *
    * @return related variable selection method.
    */
  def largest[A <: jc.IntVar] = new LargestMax[A]

  /** Wrapper for [[org.jacop.search.MaxRegret]].
    *
    * @return related variable selection method.
    */
  def maxRegret[A <: jc.IntVar] = new MaxRegret[A]

  /** Wrapper for [[org.jacop.search.IndomainMin]].
    *
    * @return related variable selection method.
    */
  def indomainMin[A <: jc.IntVar] = new IndomainMin[A]

  /** Wrapper for [[org.jacop.search.IndomainMax]].
    *
    * @return related variable selection method.
    */
  def indomainMax[A <: jc.IntVar] = new IndomainMax[A]

  /** Wrapper for [[org.jacop.search.IndomainMiddle]].
    *
    * @return related variable selection method.
    */
  def indomainMiddle[A <: jc.IntVar] = new IndomainMiddle[A]

  /** Wrapper for [[org.jacop.search.IndomainMedian]].
    *
    * @return related variable selection method.
    */
  def indomainMedian[A <: jc.IntVar] = new IndomainMedian[A]

  /** Wrapper for [[org.jacop.search.IndomainRandom]].
    *
    * @return related variable selection method.
    */
  def indomainRandom[A <: IntVar](r: util.Random = new util.Random()) = new IndomainRandom[A](r)

  // ---- Set specific ----

  /** Wrapper for [[org.jacop.set.search.MinCardDiff]].
    *
    * @return related variable selection method.
    */
  def firstFailSet[A <: jset.SetVar] = new MinCardDiff[A]

  /** Wrapper for [[org.jacop.search.MostConstrainedStatic]].
    *
    * @return related variable selection method.
    */
  def mostConstrainedSet[A <: jset.SetVar] = new MostConstrainedStatic[A]

  /** Currently equivalent to `minGLBCard`.
    *
    * @return related variable selection method.
    */
  def smallestSet[A <: jset.SetVar]: MinGlbCard[A] = minGLBCard

  /** Wrapper for [[org.jacop.set.search.MinGlbCard]].
    *
    * @return related variable selection method.
    */
  def minGLBCard[A <: jset.SetVar] = new MinGlbCard[A]

  /** Wrapper for [[org.jacop.set.search.MinLubCard]].
    *
    * @return related variable selection method.
    */
  def minLUBCard[A <: jset.SetVar] = new MinLubCard[A]

  /** Wrapper for [[org.jacop.set.search.MaxCardDiff]].
    *
    * @return related variable selection method.
    */
  def antiFirstFailSet[A <: jset.SetVar] = new MaxCardDiff[A]

  /** Wrapper for [[org.jacop.set.search.IndomainSetMin]].
    *
    * @return related indomain method.
    */
  def indomainMinSet[A <: jset.SetVar] = new IndomainSetMin[A]

  /** Wrapper for [[org.jacop.set.search.IndomainSetMax]].
    *
    * @return related indomain method.
    */
  def indomainMaxSet[A <: jset.SetVar] = new IndomainSetMax[A]

  /** Wrapper for [[org.jacop.set.search.IndomainSetRandom]].
    *
    * @return related indomain method.
    */
  def indomainRandomSet[A <: jset.SetVar] = new IndomainSetRandom[A]
}
