/*
 *  IntSet.scala
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

import org.jacop.{core => jc}

object IntSet {
  /** Creates a new ordered empty set of integers. */
  def apply(): IntSet = new IntSet

  /** Creates a new ordered set of integers.
    *
    * @param min minimal value of a set interval.
    * @param max maximal value of a set interval.
    */
  def apply(min: Int, max: Int): IntSet = {
    val res = apply()
    res.addDom(new jc.IntervalDomain(min, max))
    res
  }

  /** Creates a new ordered set containing one element.
    *
    * @param el element of set.
    */
  def apply(el: Int): IntSet = {
    val res = apply()
    res.addDom(new jc.IntervalDomain(el, el))
    res
  }
}
/** Defines an ordered set of integers and basic operations on these sets. */
class IntSet private() extends jc.IntervalDomain {
  /** Set union operation on a set and a set with one value.
    *
    * @param n element of set.
    */
  def + (n: Int): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    tmp.unionAdapt(n)
    tmp
  }

  /** Set union operation on two sets.
    *
    * @param that set variable.
    */
  def + (that: IntSet): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    tmp.unionAdapt(that)
    tmp
  }

  /** Set intersection operation on a set and a set with one value.
    *
    * @param n element of set.
    */
  def * (n: Int): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    tmp.intersectAdapt(n,n)
    tmp
  }

  /** Set intersection operation on two sets.
    *
    * @param that set variable.
    */
  def * (that: IntSet): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    tmp.intersectAdapt(that)
    tmp
  }

  /** Set subtraction constraint on a set variable and a set of one value.
    *
    * @param n element of set.
    */
  def \ (n: Int): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    tmp.subtractAdapt(n)
    tmp
  }

  /** Set subtraction operation on a set and a set with one value.
    *
    * @param that element of set.
    */
  def \ (that: IntSet): IntSet = {
    val tmp = IntSet()
    tmp.unionAdapt(this)
    for (i <- 0 until that.size) {
      tmp.subtractAdapt(that.intervals(i).min, that.intervals(i).max)
    }
    tmp
  }

  /** Set complement operation on a set. */
  def unary_~ : IntSet = {
    val tmp = IntSet(jc.IntDomain.MinInt, jc.IntDomain.MaxInt)
    for (i <- 0 until this.size)
      tmp.subtractAdapt(intervals(i).min, intervals(i).max)
    tmp
  }

  override def toString: String = if (singleton) value.toString else super.toString
}
