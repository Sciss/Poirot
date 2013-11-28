package de.sciss.poirot
package examples

import java.lang.management.ManagementFactory

/** A Social Golfer example based on set variables.
  *
  * @author Krzysztof Kuchcinski, clean up by H. H. Rutz
  */
object SocialGolfer extends Problem {
  var golferGroup: Array[Array[SetVar]] = null

  def main(args: Array[String]): Unit = {
    run(3,2,2)
    run(2,5,4)
    run(2,6,4)
    run(2,7,4)
    run(3,5,4)
    run(3,6,4)
    run(3,7,4)
    run(4,5,4)
    run(4,6,5)
    run(4,7,4)
    run(4,9,4)
    run(5,5,3)
    run(5,7,4)
    run(5,8,3)
    run(6,6,3)
    run(5,3,2)
    run(4,3,3)
  }

  def run(weeks: Int, groups: Int, players: Int): Unit = {
    val N       = groups * players
    val weights = new Array[Int](players)
    val base    = math.max(10, players + 1) // at least players + 1

    weights(players - 1) = 1

    var i = players - 2
    while (i >= 0) {
      weights(i) = weights(i + 1) * base
      i -= 1
    }

    println("\nSocial golfer problem " + weeks + "-" + groups + "-" + players)

    golferGroup = Array.tabulate(weeks, groups)((i, j) => new SetVar("g_" + i + "_" + j, 1, N))

    Array.tabulate(weeks, groups)((i, j) => card(golferGroup(i)(j)) #= players)

    for (i <- 0 until weeks; j <- 0 until groups)
      for (k <- j + 1 until groups)
        golferGroup(i)(j) <> golferGroup(i)(k)


    for (i <- 0 until weeks) {

      var t = golferGroup(i)(0)

      for (j <- 1 until groups)
        t = t + golferGroup(i)(j)

      t #= IntSet(1, N)
    }

    for (i <- 0 until weeks)
      for (j <- i + 1 until weeks)
        if (i != j)
          for (k <- 0 until groups)
            for (l <- 0 until groups)
              card(golferGroup(i)(k) * golferGroup(j)(l)) #<= 1

    val v: Array[IntVar] = new Array[IntVar](weeks)
    val var1 = List.tabulate(weeks, players)((i, j) => IntVar("var" + i + "-" + j, 1, N))

    for (i <- 0 until weeks) {
      matching(golferGroup(i)(0), var1(i))
      v(i) = weightedSum(var1(i), weights.toIndexedSeq)
    }

    for (i <- 0 until weeks - 1)
      v(i) #<= v(i + 1)

    val vars = golferGroup.flatten.toList

    def printSolution(): Unit =
      for (i <- 0 until weeks) {
        for (j <- 0 until groups) {
          print(golferGroup(i)(j).dom() + " ")
        }
        println()
      }

    def solve(): Unit = {
      val thread  = Thread.currentThread()
      val b       = ManagementFactory.getThreadMXBean

      val startCPU  = b.getThreadCpuTime  (thread.getId)
      val startUser = b.getThreadUserTime (thread.getId)

      val (result, stats) = withStatistics {
        satisfy(search(vars, minLUBCard, indomainMinSet), printSolution)
      }

      println(if (result) "*** Yes" else "*** No")

      println(stats)

      println("ThreadCpuTime  = " + (b.getThreadCpuTime (thread.getId) - startCPU ) / 1e+6 + "ms")
      println("ThreadUserTime = " + (b.getThreadUserTime(thread.getId) - startUser) / 1e+6 + "ms")
    }

    solve()
  }
}