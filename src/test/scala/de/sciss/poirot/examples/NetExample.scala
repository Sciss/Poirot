package de.sciss.poirot
package examples

import Network.Node

import Implicits._

/** @author Krzysztof Kuchcinski, clean up by H. H. Rutz */
object NetExample extends App {
  var vars: Array[IntVar] = null
  var COST: IntVar        = null

  simpleNet()
  transportationProblem()
  assignment()

  def simpleNet(): Unit = {
    implicit val model = Model()
    
    var T1: Long = 0
    var T2: Long = 0 
    var T : Long = 0
    T1 = System.currentTimeMillis()

    val x = new Array[IntVar](8)

    var net = Network()

    val source  = Node("source",  5)
    val sink    = Node("sink"  , -5)

    val A = Node("A", 0)
    val B = Node("B", 0)
    val C = Node("C", 0)
    val D = Node("D", 0)

    net = net + source + sink + A + B + C + D

    x(0) = IntVar("x_0", 0, 5)
    x(1) = IntVar("x_1", 0, 5)

    net.arc(source, A, 0, x(0))
    net.arc(source, C, 0, x(1))


    x(2) = IntVar("a->b", 0, 5)
    x(3) = IntVar("a->d", 0, 5)
    x(4) = IntVar("c->b", 0, 5)
    x(5) = IntVar("c->d", 0, 5)
    net.arc(A, B, 3, x(2))
    net.arc(A, D, 2, x(3))
    net.arc(C, B, 5, x(4))
    net.arc(C, D, 6, x(5))


    x(6) = IntVar("x_6", 0, 5)
    x(7) = IntVar("x_7", 0, 5)
    net.arc(B, sink, 0, x(6))
    net.arc(D, sink, 0, x(7))

    val cost = IntVar("cost", 0, 1000)
    net.cost(cost)
    COST = cost

    networkFlow(net)

    vars = x

    maxNumSolutions = 3
    val (success, stats) = withStatistics {
      minimizeSeq(List(
          search(x.toList, inputOrder, indomainMin),
          search(List(cost), inputOrder, indomainMin)
        ),
        cost,
        printCost, printSol)
    }

    println(stats)

    if (success) {
      println("*** Yes")
      println(cost)
    }
    else
      println("*** No")

    T2 = System.currentTimeMillis()
    T  = T2 - T1
    println("\n\t*** Execution time = " + T + " ms")
  }

  def transportationProblem(): Unit = {
    implicit val model = Model()
    
    var T1: Long = 0
    var T2: Long = 0
    var T : Long = 0
    T1 = System.currentTimeMillis()

    var net = Network()

    val A = Node("A", 0)
    val B = Node("B", 0)
    val C = Node("C", 0)
    val D = Node("D", 0)
    val E = Node("E", 0)
    val F = Node("F", 0)


    val source = Node("source", 9) // should ne 5+3+3=11 but it does not work...

    val sinkD = Node("sinkD", -3)
    val sinkE = Node("sinkE", -3)
    val sinkF = Node("sinkF", -3)

    net = net + source + sinkD + sinkE + sinkF + A + B + C + D + E + F

    val x = new Array[IntVar](13)

    x(0) = IntVar("x_0", 0, 5)
    x(1) = IntVar("x_1", 0, 3)
    x(2) = IntVar("x_2", 0, 3)
    net.arc(source, A, 0, x(0))
    net.arc(source, B, 0, x(1))
    net.arc(source, C, 0, x(2))

    x(3) = IntVar("a->d", 0, 5)
    x(4) = IntVar("a->e", 0, 5)
    net.arc(A, D, 3, x(3))
    net.arc(A, E, 1, x(4))

    x(5) = IntVar("b->d", 0, 3)
    x(6) = IntVar("b->e", 0, 3)
    x(7) = IntVar("b->f", 0, 3)
    net.arc(B, D, 4, x(5))
    net.arc(B, E, 2, x(6))
    net.arc(B, F, 4, x(7))

    x(8) = IntVar("c->e", 0, 3)
    x(9) = IntVar("c->f", 0, 3)
    net.arc(C, E, 3, x(8))
    net.arc(C, F, 3, x(9))

    x(10) = IntVar("x_10", 3, 3)
    x(11) = IntVar("x_11", 3, 3)
    x(12) = IntVar("x_12", 3, 3)
    net.arc(D, sinkD, 0, x(10))
    net.arc(E, sinkE, 0, x(11))
    net.arc(F, sinkF, 0, x(12))

    val cost = IntVar("cost", 0, 1000)
    net.cost(cost)
    COST = cost

    networkFlow(net)

    vars = x

    val (success, stats) = withStatistics {
      minimizeSeq(List(
          search(x.toList  , inputOrder, indomainMin),
          search(List(cost), inputOrder, indomainMin)
        ),
        cost,
        printCost, printSol)
    }
    
    println(stats)

    if (success) {
      println("*** Yes")
      println(cost)
    }
    else
      println("*** No")

    T2 = System.currentTimeMillis()
    T = T2 - T1
    println("\n\t*** Execution time = " + T + " ms")
  }

  def assignment(): Unit = {
    implicit val model = Model()

    var T1: Long = 0
    var T2: Long = 0
    var T : Long = 0
    T1 = System.currentTimeMillis()

    var net = Network()

    val A  = Node("A", 1)
    val B  = Node("B", 1)
    val C  = Node("C", 1)
    val D  = Node("D", 1)
    val n1 = Node("n1", -1)
    val n2 = Node("n2", -1)
    val n3 = Node("n3", -1)
    val n4 = Node("n4", -1)

    net = net + A + B + C + D + n1 + n2 + n3 + n4

    val x = new Array[IntVar](12)

    x(0) = IntVar("a->2", 0, 1)
    x(1) = IntVar("a->3", 0, 1)
    x(2) = IntVar("a->4", 0, 1)
    net.arc(A, n2, 9, x(0))
    net.arc(A, n3, 7, x(1))
    net.arc(A, n4, 13, x(2))

    x(3) = IntVar("b->1", 0, 1)
    x(4) = IntVar("b->2", 0, 1)
    x(5) = IntVar("b->3", 0, 1)
    net.arc(B, n1, 16, x(3))
    net.arc(B, n2, 13, x(4))
    net.arc(B, n3, 8, x(5))

    x(6) = IntVar("c->1", 0, 1)
    x(7) = IntVar("c->2", 0, 1)
    x(8) = IntVar("c->4", 0, 1)
    net.arc(C, n1, 10, x(6))
    net.arc(C, n3, 6, x(7))
    net.arc(C, n4, 15, x(8))

    x( 9) = IntVar("d->1", 0, 1)
    x(10) = IntVar("d->2", 0, 1)
    x(11) = IntVar("d->3", 0, 1)
    net.arc(D, n1, 11, x(9))
    net.arc(D, n3, 13, x(10))
    net.arc(D, n4, 17, x(11))

    val cost = IntVar("cost", 0, 1000)
    net.cost(cost)
    COST = cost

    networkFlow(net)

    vars = x

    // numberSolutions(2)
    val (success, stats) = withStatistics {
      minimizeSeq(List(
          search(x.toList  , inputOrder, indomainMin),
          search(List(cost), inputOrder, indomainMin)
        ),
        cost,
        printCost, printSol)
    }
    println(stats)

    if (success) {
      println("*** Yes")
      println(cost)
    }
    else
      println("*** No")

    T2 = System.currentTimeMillis()
    T = T2 - T1
    println("\n\t*** Execution time = " + T + " ms")
  }

  def printCost(): Unit = println(COST)
  def printSol (): Unit = println(vars.toList)
}
