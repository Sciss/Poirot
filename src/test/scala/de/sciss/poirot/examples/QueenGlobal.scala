package de.sciss.poirot
package examples

object QueenGlobal extends App with Problem {

  val n = 100

  val q = Array.tabulate(n)( i => new IntVar("q"+i, 0, n) )
  allDifferent(q: _*)

  val q1 = Array.tabulate(n)( i => q(i) + i )
  allDifferent(q1: _*)

  val q2 = Array.tabulate(n)( i => q(i) - i )
  allDifferent(q2: _*)
  
  val result = satisfy(search(q.toList, firstFail, indomainMiddle))

  if (result) println("Yes")
  else println("No solution")
}
