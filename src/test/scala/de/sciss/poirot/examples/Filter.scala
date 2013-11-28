package de.sciss.poirot
package examples

import collection.immutable.{IndexedSeq => Vec}

object Filter extends App with Problem {
  val addId = 1
  val mulId = 2
  val dependencies = Vec(
    Vec( 0,  2), Vec( 0, 15), Vec( 0, 17), Vec( 1,  4),
    Vec( 1,  8), Vec( 1, 11), Vec( 2,  3), Vec( 2,  7), Vec( 2,  9), Vec( 3,  4),
    Vec( 4,  5), Vec( 4,  6), Vec( 4, 10), Vec( 5,  7), Vec( 6,  8), Vec( 7,  9),
    Vec( 7, 10), Vec( 8, 11), Vec( 8, 13), Vec( 8, 19), Vec( 9, 12), Vec(10, 13),
    Vec(11, 14), Vec(12, 15), Vec(14, 16), Vec(15, 17), Vec(15, 18),
    Vec(15, 29), Vec(16, 20), Vec(16, 28), Vec(16, 19), Vec(17, 21),
    Vec(18, 22), Vec(19, 23), Vec(20, 24), Vec(21, 27), Vec(22, 25),
    Vec(22, 32), Vec(23, 26), Vec(23, 33), Vec(16, 28), Vec(24, 28),
    Vec(25, 30), Vec(26, 31), Vec(27, 29), Vec(30, 32), Vec(31, 33))

  val ids = Vec(
    addId, addId, addId, addId, addId, mulId, mulId, addId,
    addId, addId, addId, addId, mulId, addId, mulId, addId, addId,
    addId, addId, addId, addId, mulId, addId, addId, mulId, mulId,
    mulId, addId, addId, addId, addId, addId, addId, addId)

  val last = Vec(13, 24, 28, 29, 30, 31, 32, 33)

  val delAdd = IntVar("delAdd", 1,1)
  val delMul = IntVar("delMul", 2,2)

  val t   = Vec.tabulate(ids.length)(i => IntVar("t" + i, 0, 100))
  val r   = Vec.tabulate(ids.length)(i => if (ids(i) == addId) IntVar("r" + i, 1, 2) else IntVar("r" + i, 3, 4))
  val del = Vec.tabulate(ids.length)(i => if (ids(i) == addId) delAdd else delMul)

  for (i <- 0 until dependencies.length)
    t(dependencies(i)(0)) + del(dependencies(i)(0)) #<= t(dependencies(i)(1))

  val endOps  = List.tabulate(last.length)(i => t(last(i)) + del(last(i)))
  val end     = max(endOps: _*)

  //   val one = new IntVar("1", 1,1)
  val one = IntVar(1, 1)
  val rectangles: Vec[Vec[IntVar]] = Vec.tabulate(ids.length)(i => Vec(t(i), r(i), del(i), one))
  diff2(rectangles)

  //   val tAdd = for (i <- Vec.range(0, t.length); if (ids(i) == addId) ) yield t(i)
  //   val dAdd = for (i <- Vec.range(0, del.length); if (ids(i) == addId) ) yield del(i)
  //   val rAdd = for (i <- Vec.range(0, r.length); if (ids(i) == addId) ) yield one
  //   val limitAdd = new IntVar("limitAdd", 0, 2)
  //   cumulative(tAdd, dAdd, rAdd, limitAdd)

  //   val tMul = for (i <- Vec.range(0, t.length); if (ids(i) == mulId) ) yield t(i)
  //   val dMul = for (i <- Vec.range(0, del.length); if (ids(i) == mulId) ) yield del(i)
  //   val rMul = for (i <- Vec.range(0, r.length); if (ids(i) == mulId) ) yield one
  //   val limitMul = new IntVar("limitMul", 0, 2)
  //   cumulative(tMul, dMul, rMul, limitMul)

  // Search
  val tr = Vec.tabulate(t.length)(i => Vec(t(i), r(i)))

  // numberSolutions(2)
  val (result, stats) = withStatistics {
    minimize(searchVector(tr, smallestMin, indomainMin), end, printSol)
  }
  // numberSolutions(2)
  // val result = minimize_seq( List(search(t, smallest_min, indomain_min), search(r, input_order, indomain_min)), 
  // end, printSol )

  println(stats)

  def printSol(): Unit = {
    println("\nSolution with cost: " + end.value + "\n=======================")
    println(tr)
  }
}
