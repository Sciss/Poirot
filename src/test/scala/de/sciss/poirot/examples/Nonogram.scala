package de.sciss.poirot
package examples

import java.io.BufferedReader
import java.io.FileNotFoundException
import java.io.FileReader
import java.io.IOException
import scala.collection.mutable
import org.jacop.constraints.ExtensionalSupportMDD

/** Solves a nonogram example problem, sometimes also called Paint by Numbers.
  *
  * XXX TODO: code is a mess, needs clean up
  *
  * @author Radoslaw Szymanek; rewriting to Scala Krzysztof Kuchcinski; clean up and adaptation H. H. Rutz
  */
object Nonogram extends Problem {
  /** The value that represents a black dot. */
  val black = 1

  /** The value that represents a white dot. */
  val white = 0

  /** A board to be painted in white/black dots. */
  var board: Array[Array[IntVar]] = null

  /** Specifies if the slide based decomposition of the regular constraint
    * should be applied. This decomposition uses ternary extensional support
    * constraints. It achieves GAC if FSM is deterministic.
    */
  val slideDecomposition = false

  /** Specifies if the regular constraint should be used. */
  val regularConstr = true

  /** Specifies if one extensional constraint based on MDD created from FSM
    * should be used. The translation process works if FSM is deterministic.
    */
  val extensionalMDD = false

  def readFromFile(filename: String): Unit = {

    var lines       = new Array[String](100)
    val dimensions  = new Array[Int](2)

    // read from file args[0] or qcp.txt
    try {
      val in      = new BufferedReader(new FileReader(filename))
      var str     = in.readLine()
      val result  = str.split(" ")

      var current = 0

      for (j <- 0 until result.length) {
        val currentNo = result(j).toInt
        dimensions(current) = currentNo
        current += 1
      }

      lines = new Array[String](dimensions(0) + dimensions(1))

      var n = 0

      str = in.readLine()
      while (str != null && n < lines.length) {
        lines(n) = str
        n += 1
        str = in.readLine()
      }
      in.close()
    } catch {
      case _: FileNotFoundException =>
        Console.err.println("I can not find file " + filename)
      case _: IOException =>
        Console.err.println("Something is wrong with file" + filename)

        rowRules = new Array[Array[Int]](dimensions(1))
        colRules = new Array[Array[Int]](dimensions(0))

        // Transforms strings into ints
        for (i <- lines.indices) {

          val result = lines(i).split(" ")

          val sequence = new Array[Int](result.length)

          var current = 0
          for (j <- 0 until result.length)
            try {
              sequence(current) = result(j).toInt
              current += 1
            } catch {
              case _: Exception =>
            }
          if (i < rowRules.length) rowRules(i) = sequence
          else
            colRules(i - rowRules.length) = sequence
        }

    }
  }


  /** Produces an FSM given a sequence representing a rule. e.g. [2, 3]
    * specifies that there are two black dots followed by three black dots.
    *
    * @return Finite State Machine used by Regular automaton to enforce proper sequence.
    */
  def createAutomaton(sequence: Seq[Int]): FSM = {
    import FSM.State

    var result = FSM()

    var currentState = State()
    result.init(currentState)

    currentState ~> (white, currentState)

    for (i <- sequence.indices) {
      if (sequence(i) != 0) {
        for (_ <- 0 until sequence(i)) {
          // Black transition
          val nextState = State()
          result       += nextState
          currentState ~> (black, nextState)
          currentState  = nextState
        }
        // White transitions
        if (i + 1 != sequence.length) {
          val nextState = State()
          result       += nextState
          currentState ~> (white, nextState)
          currentState  = nextState
        }
        currentState ~> (white, currentState)
      }
    }

    result.addFinalStates(currentState)

    result
  }

  def run(): Unit = {
    import org.jacop.constraints.regular.Regular

    val vars = mutable.Buffer.empty[IntVar]

    val values = IntSet() + white + black

    // Specifying the board with allowed values.
    board = Array.tabulate(rowRules.length, colRules.length)((i, j) =>
      IntVar("board[" + i + "][" + j + "]", values))

    // Zigzag based variable ordering.
    for (m <- 0 until rowRules.length + colRules.length - 1) {
      for (j <- 0 until m if j < colRules.length) {
        val i = m - j
        if (i < rowRules.length)
          vars += board(i)(j)
      }
    }

    println("Size " + vars.length)

    // Making sure that rows respect the rules.
    for (i <- rowRules.indices) {

      val result = createAutomaton(rowRules(i))

      if (slideDecomposition)
        model.imposeDecomposition(new Regular(result, board(i).asInstanceOf[Array[org.jacop.core.IntVar]]))

      if (regularConstr)
        regular(result, board(i).toList)

      if (extensionalMDD)
        model.impose(new ExtensionalSupportMDD(result.transformDirectlyIntoMDD(board(i).asInstanceOf[Array[org.jacop.core.IntVar]])))

    }

    // Making sure that columns respect the rules.
    for (i <- colRules.indices) {

      val result = createAutomaton(colRules(i))
      val column = Array.tabulate(rowRules.length)(j => board(j)(i))

      if (slideDecomposition)
        model.imposeDecomposition(new Regular(result, column.asInstanceOf[Array[org.jacop.core.IntVar]]))

      if (regularConstr)
        regular(result, column.toList)

      if (extensionalMDD)
        model.impose(new ExtensionalSupportMDD(result.transformDirectlyIntoMDD(column.asInstanceOf[Array[org.jacop.core.IntVar]])))
    }

    /* val solved = */ satisfy(search(vars.toList, inputOrder, indomainMin))
  }

  /** Prints a matrix of variables. All variables must be grounded.
    *
    * @param matrix matrix containing the grounded variables.
    */
  def printMatrix(matrix: Array[Array[IntVar]]): Unit = {
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        if (matrix(i)(j).value() == black)
          print("0")
        else
          print(" ")
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    run()
    printMatrix(board)
  }

  /** Specifies a rule for each row. */
  var rowRules = Array(
		  Array(0,0,0,0,2,2,3),
		  Array(0,0,4,1,1,1,4),
		  Array(0,0,4,1,2,1,1),
		  Array(4,1,1,1,1,1,1),
		  Array(0,2,1,1,2,3,5),
		  Array(0,1,1,1,1,2,1),
		  Array(0,0,3,1,5,1,2),
		  Array(0,3,2,2,1,2,2),
		  Array(2,1,4,1,1,1,1),
		  Array(0,2,2,1,2,1,2),
		  Array(0,1,1,1,3,2,3),
		  Array(0,0,1,1,2,7,3),
		  Array(0,0,1,2,2,1,5),
		  Array(0,0,3,2,2,1,2),
		  Array(0,0,0,3,2,1,2),
		  Array(0,0,0,0,5,1,2),
		  Array(0,0,0,2,2,1,2),
		  Array(0,0,0,4,2,1,2),
		  Array(0,0,0,6,2,3,2),
		  Array(0,0,0,7,4,3,2),
		  Array(0,0,0,0,7,4,4),
		  Array(0,0,0,0,7,1,4),
		  Array(0,0,0,0,6,1,4),
		  Array(0,0,0,0,4,2,2),
		  Array(0,0,0,0,0,2,1)
	)

  /** Specifies a rule for each column. */
  var colRules = Array(
		   Array(0,0,1,1,2,2),
		   Array(0,0,0,5,5,7),
		   Array(0,0,5,2,2,9),
		   Array(0,0,3,2,3,9),
		   Array(0,1,1,3,2,7),
		   Array(0,0,0,3,1,5),
		   Array(0,7,1,1,1,3),
		   Array(1,2,1,1,2,1),
		   Array(0,0,0,4,2,4),
		   Array(0,0,1,2,2,2),
		   Array(0,0,0,4,6,2),
		   Array(0,0,1,2,2,1),
		   Array(0,0,3,3,2,1),
		   Array(0,0,0,4,1,15),
		   Array(1,1,1,3,1,1),
		   Array(2,1,1,2,2,3),
		   Array(0,0,1,4,4,1),
		   Array(0,0,1,4,3,2),
		   Array(0,0,1,1,2,2),
		   Array(0,7,2,3,1,1),
		   Array(0,2,1,1,1,5),
		   Array(0,0,0,1,2,5),
		   Array(0,0,1,1,1,3),
		   Array(0,0,0,4,2,1),
		   Array(0,0,0,0,0,3)
	)
}
