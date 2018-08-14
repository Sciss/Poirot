package de.sciss.poirot
package examples

import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException}
import java.util.StringTokenizer

import scala.collection.{breakOut, mutable}

/** Solves the Mixed Multi-Unit Combinatorial Auctions.
  *
  * @author Radoslaw Szymanek (for Scala Krzysztof Kuchcinski)
  * @version 3.0
  *
  *
  * The idea originated from reading the following paper
  * where the first attempt to use CP was presented.
  *
  * Comparing Winner Determination Algorithms for Mixed
  * Multi-Unit Combinatorial Auctions by Brammert Ottens
  * Ulle Endriss
  */

object MUCA extends Problem {

  /**
   * ArrayBuffer of bids issued by different bidders. 
   * Each bidder issues an ArrayBuffer of xor bids. 
   * Each Xor bid is a list of transformations. 
   */
  var bids: mutable.Buffer[mutable.Buffer[mutable.Buffer[Transformation]]] = _

  /**
   * For each bidder and each xor bid there is an 
   * integer representing a cost of the xor bid.
   */
  var costs: mutable.Buffer[mutable.Buffer[Int]] = _

  /**
   * It specifies the initial quantities of goods.
   */
  var initialQuantity: List[Int] = List()

  /**
   * It specifies the minimal quantities of items seeked to achieve.
   */
  var finalQuantity : List[Int] = List()

  /**
   * It specifies number of goods which are in the focus of the auction.
   */
  var noGoods = 7

  /**
   * It specifies the minimal possible delta of goods for any transformation.
   */
  var minDelta = -10
	
  /**
   * It specifies the maximal possible delta of goods for any transformation.
   */
  
  var maxDelta = 10

  /**
   * It specifies the minimal value for the cost.
   */
  val minCost = -100000

  /**
   * It specifies the maximal value for the cost.
   */
  val maxCost = 100000

  var cost: IntVar = _
	
  /**
   * The maximal number of products.
   */
  val maxProducts = 100

  /**
   * For each bidder it specifies variable representing 
   * the cost of the chosen xor bid.
   */
  var bidCosts: List[IntVar] = _
	
  /**
   * It specifies the sequence of transitions used by an auctioneer.
   */
  var transitions: Vec[IntVar] = _

  /**
   * It specifies the maximal number of transformations used by the auctioneer.
   */
  var maxNoTransformations : Int = 0
	
  /**
   * For each transition and each good it specifies the 
   * delta change of that good before the transition takes place.
   */
  var deltasI : Array[Array[IntVar]] = _

  /**
   * For each transition and each good it specifies the 
   * delta change of that good after the transition takes place.
   */	
  var deltasO : Array[Array[IntVar]] = _

  /**
   * It specifies the number of goods after the last transition.
   */
  var summa : Array[IntVar] = _

  /**
   * It executes the program which solve the supplied auction problem or
   * solves three problems available within the files. 
   * 
   * @param args the first argument specifies the name of the file containing the problem description.
   */
  def main(args: Array[String]): Unit = {
    val filename = args.headOption.getOrElse {
      s"${sys.props("user.home")}/testset3.auct"
    }

    run(filename)
  }


  def run(filename: String): Unit = {

    readAuction(filename)

    // maximal number of transformations in a sequence.
    maxNoTransformations = 0
    // number of transformations
    var noAvailableTransformations = 0

    for (bid <- bids) {
      var max = 0

      for (bidXor <- bid) {

        noAvailableTransformations += bidXor.length
        if (bidXor.length > max)
          max = bidXor.length
      }

      maxNoTransformations += max
    }

    // Variables, transition ordering
    transitions = Vec.tabulate(maxNoTransformations)(i => IntVar("t" + (i + 1), 0, noAvailableTransformations))

    for (i <- 0 until maxNoTransformations - 1) {
      val b = BooleanVar("b" + i)
      b #<-> (transitions(i)     #= 0)
      b #->  (transitions(i + 1) #= 0)
    }

    // for each set of transformations create an among

    val usedTransformation = Array.tabulate(noAvailableTransformations)(i => IntVar("isUsed_" + (i + 1), 0, 1))

    for (i <- 0 until noAvailableTransformations)
      among(transitions, IntSet(i + 1, i + 1), usedTransformation(i))

    var noTransformations = 0
    var no = 0

    var usedXorBids = mutable.Buffer.empty[IntVar]

    bidCosts = List[IntVar]()

    for (bid <- bids) {
      val tuplesTail: Vec[(IntVar, Vec[Int])] = bid.zipWithIndex.map { case (bidXor, i) =>
        var kSet = IntSet()

        var xorUsedTransformation = Vec.empty[IntVar]
        for (t <- bidXor) {
          noTransformations += 1
          t.id = noTransformations
          // + is set union ;)
          kSet += noTransformations
          xorUsedTransformation :+= usedTransformation(t.id - 1)
        }

        val n = IntVar("ind_" + no + "_" + i, 0, 0)
        n.addDom(bidXor.length, bidXor.length)

        sum(xorUsedTransformation) #= n

        usedXorBids :+= n

        // val tuples(i) = new Array[Int](bid.length + 1)
        val tup = Vec.tabulate(bid.size + 1) {
          case 0                => costs(no)(i)
          case j if j == i + 1  => n.max()
          case _                => 0
        }

        among(transitions, kSet, n)
        
        (n, tup)
      } (breakOut)

      val bidCost = IntVar("bidCost" + (bidCosts.length + 1), minCost, maxCost)
      
      val tuples  = (bidCost, Vec.fill(bid.length + 1)(0)) +: tuplesTail
      
      table(tuples)

      bidCosts :+= bidCost

      no += 1
    }

    deltasI = Array.ofDim(maxNoTransformations, noGoods)
    deltasO = Array.ofDim(maxNoTransformations, noGoods)

    summa = new Array[IntVar](noGoods)

    for (g <- 0 until noGoods) {

      var tuples4transitions = mutable.Buffer.empty[Array[Int]]
      val dummyTransition = Array[Int](0, 0, 0)
      tuples4transitions :+= dummyTransition

      bids.foreach(bid =>
        bid.foreach(bidXor =>
          bidXor.foreach(t =>
            tuples4transitions :+= Array[Int](t.id, -t.getDeltaInput(g), t.getDeltaOutput(g)))))

      val tuples = tuples4transitions.map(_.toIndexedSeq)

      var previousPartialSum = IntVar("initialQuantity_" + g, initialQuantity(g), initialQuantity(g))

      for (i <- 0 until maxNoTransformations) {

        var vars = Vec[IntVar]()
        vars :+= transitions(i)
        deltasI(i)(g) = IntVar("deltaI_g" + g + "t" + i, minDelta, maxDelta)
        vars :+= deltasI(i)(g)
        deltasO(i)(g) = IntVar("deltaO_g" + g + "t" + i, minDelta, maxDelta)
        vars :+= deltasO(i)(g)

        table(vars zip tuples)

        (previousPartialSum + deltasI(i)(g)) #> -1

        val partialSum = IntVar("partialSum_" + g + "_" + i, 0, maxProducts)
        previousPartialSum + deltasI(i)(g) + deltasO(i)(g) #= partialSum

        previousPartialSum = partialSum
      }

      previousPartialSum #>= finalQuantity(g)
      summa(g) = previousPartialSum
    }


    for (g <- 0 until noGoods) {

      var weights = Vec.empty[IntVar]
      weights = weights updated (0, IntVar(s"${initialQuantity(g)}of-g$g", initialQuantity(g), initialQuantity(g)))

      for (bid <- bids) {
        for (bidXor <- bid) {
          for (t <- bidXor) {
            weights = weights updated (t.id, if (t.getDelta(g) >= 0)
              IntVar("delta_tid_" + t.id + "_g" + g, 0, t.getDelta(g))
            else
              IntVar("delta_t" + t.id + "_g", t.getDelta(g), 0)
            )
            val vars    = List(
              usedTransformation(t.id - 1) -> List(0, 0),
              weights           (t.id    ) -> List(1, t.getDelta(g))
            )
            table(vars)
          }
        }
      }
      sum(weights.toList) #= summa(g)
    }

    cost = sum(bidCosts)

    searchSpecial()
  }

  /** Executes special master-slave search. The master search
    * uses costs variables and maxregret criteria to choose an
    * interesting bids. The second search (slave) looks for the
    * sequence of chosen transactions such as that all constraints
    * concerning goods quantity (deltas of transitions) are respected.
    *
    * @return true if there is a solution, false otherwise.
    */
  def searchSpecial(): Boolean = {

    val search1 = search(bidCosts           , maxRegret , indomainMin)
    val search2 = search(transitions.toList , inputOrder, indomainMin)

    val result = minimizeSeq(List(search1, search2), cost)

    print("\t")

    for (i <- 0 until maxNoTransformations if transitions(i).value() != 0 ) print(transitions(i) + "\t")
    println()

    for (g <- 0 until noGoods) {
      print(initialQuantity(g) + "\t")
      for (i <- 0 until maxNoTransformations if transitions(i).value() != 0)
        print(deltasI(i)(g).value() + "," + deltasO(i)(g).value() + "\t")

      println(summa(g).value() + ">=" + finalQuantity(g))

    }

    result
  }

  class Delta(var input: Int, var output: Int) {
    // Both must be positive, even if input means consuming.

    // negative means consumption, positive means production.
    def this(delta: Int) = {
      this(if (delta > 0) 0 else -delta, if (delta > 0) delta else 0)
    }
  }

  class Transformation {
    var goodsIds  = mutable.Buffer.empty[Int]
    var delta     = mutable.Buffer.empty[Delta]
    var id        = 0

    def getDelta(goodId: Int): Int = {
      for (i <- goodsIds.indices)
        if (goodsIds(i) == goodId)
          return delta(i).output - delta(i).input

      0
    }

    def getDeltaInput(goodId: Int): Int = {
      for (i <- goodsIds.indices)
        if (goodsIds(i) == goodId)
          return delta(i).input

      0
    }

    def getDeltaOutput(goodId: Int): Int = {
      for (i <- goodsIds.indices)
        if (goodsIds(i) == goodId)
          return delta(i).output

      0
    }

    override def toString: String = {
      var st = "*** "
      for (i <- goodsIds.indices)
        st += "id =" + goodsIds(i) + "(" + getDeltaInput(goodsIds(i)) + ", " + getDeltaOutput(goodsIds(i)) + ") "
      st
    }
  }


  /** Reads the auction problem from the file.
    *
    * @param filename file describing the auction problem.
    */
  def readAuction(filename: String): Unit = {
    noGoods = 0

    try {
      val br = new BufferedReader(new FileReader(filename))

      // the first line represents the input goods
      var line = br.readLine()
      var tk = new StringTokenizer(line, "(),: ")

      while (tk.hasMoreTokens) {
        noGoods += 1
        tk.nextToken()
        initialQuantity :+= tk.nextToken.toInt
      }

      // the second line represents the output goods
      line = br.readLine()
      tk = new StringTokenizer(line, "(),: ")

      while (tk.hasMoreTokens) {
        tk.nextToken()
        finalQuantity :+= tk.nextToken().toInt
      }

      // until the word price is read, one is reading transformations.
      // Assume that the transformations are properly grouped

      line = br.readLine()

      var bidCounter            = 1
      var bidXorCounter         = 1
      var transformationCounter = 0
      var goodsCounter          = 0
      var Id=0; var in=0; var out=0

      var input   : Array[Int] = null
      var output  : Array[Int] = null

      bids      = mutable.Buffer.empty[mutable.Buffer[mutable.Buffer[Transformation]]]
      bids    :+= mutable.Buffer.empty[mutable.Buffer[Transformation]]
      bids(0) :+= mutable.Buffer.empty[Transformation]

      while (!line.equals("price")) {
        tk = new StringTokenizer(line, "():, ")
        transformationCounter += 1

        if (tk.nextToken().toInt > bidCounter) {
          bidCounter += 1
          bidXorCounter = 1
          transformationCounter = 1

          bids :+= mutable.Buffer[mutable.Buffer[Transformation]]()
          bids(bidCounter - 1) :+= mutable.Buffer.empty[Transformation]
        }
        // 				System.out.println(bidCounter + " " + bidXorCounter);
        if (tk.nextToken().toInt > bidXorCounter) {
          bidXorCounter       += 1
          transformationCounter = 1

          bids(bidCounter - 1) :+= mutable.Buffer.empty[Transformation]
        }
        // this token contains the number of the transformation
        tk.nextToken()
        bids(bidCounter - 1)(bidXorCounter - 1) :+= new Transformation()

        bids(bidCounter - 1)(bidXorCounter - 1)(transformationCounter - 1).goodsIds  = mutable.Buffer.empty[Int]
        bids(bidCounter - 1)(bidXorCounter - 1)(transformationCounter - 1).delta     = mutable.Buffer.empty[Delta]

        input   = new Array[Int](noGoods)
        output  = new Array[Int](noGoods)

        goodsCounter = 0
        while (tk.hasMoreTokens) {
          goodsCounter += 1
          // 	  System.out.println(goodsCounter);
          if (goodsCounter <= noGoods) {
            Id = tk.nextToken().toInt - 1
            in = tk.nextToken().toInt
            input(Id) = in
          } else {
            Id  = tk.nextToken().toInt - 1
            out = tk.nextToken().toInt
            output(Id) = out
          }
        }

        for (i <- 0 until noGoods) {
          //delta = output[i] - input[i];
          if (output(i) > maxDelta) {
            maxDelta = output(i)
          } else if (-input(i) < minDelta) {
            minDelta = -input(i)
          }

          if (output(i) != 0 || input(i) != 0) {
            // 						System.out.print(i + " " + input[i] + ":" + output[i] + " ");
            // 						System.out.println(bidCounter + " " + bidXorCounter + " " + transformationCounter + " " + i + " " + delta);
            bids(bidCounter - 1)(bidXorCounter - 1)(transformationCounter - 1).goodsIds :+= i
            bids(bidCounter - 1)(bidXorCounter - 1)(transformationCounter - 1).delta :+= new Delta(input(i), output(i))
          }
        }
        // 				System.out.print("\n");

        line = br.readLine()
      }

      // now read in the price for each xor bid

      costs = mutable.Buffer.empty[mutable.Buffer[Int]]

      costs :+= mutable.Buffer.empty[Int]

      bidCounter = 1
      line = br.readLine()

      while (!(line == null)) {
        tk = new StringTokenizer(line, "(): ")

        if (tk.nextToken().toInt > bidCounter) {
          bidCounter += 1
          costs     :+= mutable.Buffer.empty[Int]
        }

        // this token contains the xor_bid id.
        tk.nextToken()

        costs(bidCounter - 1) :+= tk.nextToken().toInt

        line = br.readLine()

      }
    }
    catch {
      case ex: FileNotFoundException =>
        Console.err.println("You need to run this program in a directory that contains the required file.")
        Console.err.println(ex)
        sys.exit(-1)

      case ex: IOException =>
        Console.err.println(ex)
        sys.exit(-1)
    }

    //     println(this.maxCost);
    //     println(this.maxDelta);
    //     println(this.minDelta);

    //     println ("bids = "+bids);
    //     println ("costs = "+costs);

  }
}