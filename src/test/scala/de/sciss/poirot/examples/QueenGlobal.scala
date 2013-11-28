package de.sciss.poirot
package examples

object QueenGlobal extends App with Problem {

  val n = 100

  val q = Vec.tabulate(n)( i => new IntVar("q"+i, 0, n) )
  q.allDifferent()

  val q1 = Vec.tabulate(n)( i => q(i) + i )
  q1.allDifferent()

  val q2 = Vec.tabulate(n)( i => q(i) - i )
  q2.allDifferent()
  
  val result = satisfy(search(q.toList, firstFail, indomainMiddle))

  if (result) println("Yes")
  else println("No solution")
}
