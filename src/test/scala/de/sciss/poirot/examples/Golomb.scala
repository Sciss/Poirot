package de.sciss.poirot
package examples

import Implicits._

object Golomb extends App with Problem {
  val start = System.currentTimeMillis()

  // Golomb
  val m = 8
  val n = m*m

  val mark = Vec.tabulate(m)(i => IntVar("mark" + i, 0, n))

  val differences =
    for {
      i <- 0   until m
      j <- i+1 until m
    } yield mark(j) - mark(i)

  differences.foreach(_ #>= 0)

  mark(0) #= 0

  for (i <- 0 until m - 1) mark(i) #< mark(i + 1)

  differences(0) #< differences(differences.length - 1) 

  differences.allDifferent()

  val result = minimize(search(mark.toList, inputOrder, indomainMin), mark(m - 1))

  val end = System.currentTimeMillis()

  if (result) {
    print("Golomb ruler : ")

    for (i <- 0 until m) print(mark(i).dom + " ")

    println("\n\n*** Execution time = " + (end - start) + " ms")

  } else {
    println("No solution")
  }
}