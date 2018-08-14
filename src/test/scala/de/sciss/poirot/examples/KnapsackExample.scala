/*
 *  KnapsackExample.scala
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
package examples

import scala.collection.mutable

/**
 * 
 * It shows the capabilities and usage of Knapsack constraint.
 * 
 * @author Krzysztof Kuchcinski & Radoslaw Szymanek
 * @version 3.0
 * 
 * It models and solves a simple knapsack problem. There 
 * are two different models. The first one uses quantity
 * from 0 to n, where the second model is allowed to use
 * only binary variables. 
 * 
 * Each item is specified by its weight and profit. Find
 * what objects should be put in the knapsack to maximize 
 * the profit without exceeding the knapsack capacity.
 * 
 */

object KnapsackExample extends App with Problem {
  var vars = mutable.Buffer.empty[IntVar]

  run()

	/** Stores the parameters of the main function to be
	  * used by the model functions.
	  */
  def run(): Unit = {
    var noItems = 3
    var volume  = 9
    var weights = Array(4, 3, 2)
    var profits = Array(15, 10, 7)
    var names   = Array("whisky", "perfumes", "cigarets")

    var maxs    = Array.tabulate(noItems)(i => volume / weights(i))

    // It is possible to supply the program
    // with the volume size and items (weight, profit, maximum_quantity,
    // name )
    if (args.length >= 5 && ((args.length - 1) % 4) == 0) {
      volume  = args(0).toInt
      noItems = (args.length - 1) / 4
      weights = new Array[Int](noItems)
      profits = new Array[Int](noItems)
      maxs    = new Array[Int](noItems)
      names   = new Array[String](noItems)
      var i = 1
      while (i < args.length) {
        weights((i - 1) / 4) = args(i).toInt
        i += 1
        profits((i - 1) / 4) = args(i).toInt
        i += 1
        maxs((i - 1) / 4) = args(i).toInt
        i += 1
        names((i - 1) / 4) = args(i)
        i += 1
      }
    }

    // I-th variable represents if i-th item is taken
    // Each quantity variable has a domain from 0 to max value
    val quantity = List.tabulate(noItems)(i => IntVar("Quantity_" + names(i), 0, maxs(i)))

    val profit = IntVar("Profit", 0, 1000000)
    val weight = IntVar("Weight", 0, 1000000)

    knapsack(profits, weights, quantity, weight, profit)
		
    weight #<= volume

    /* val result = */ maximize(search(quantity, firstFail, indomainMin), profit)
  }
}