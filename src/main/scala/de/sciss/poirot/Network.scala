package de.sciss.poirot

import org.jacop.constraints.{netflow => jnet}

/** Network specification for networkflow constraint
  *
  * @constructor Creates an empty network
  */
class Network extends jnet.NetworkBuilder {
  import Network.Node

  import scala.collection.mutable

  val nodes = mutable.Map[Node, jnet.simplex.Node]()

  /** Adds nodes to the network
    *
    * @param n node
    */
  def + (n: Node): Network = {
    val N = addNode(n.name, n.balance)
    nodes += (n -> N)
    // println("## " + N.name + ", " + N.balance)
    this
  }

  /** Gets a node of the network in network format
    *
    * @param n node
    */
  def apply(n: Node): jnet.simplex.Node = nodes(n)

  /** Creates an arc between two nodes.
    *
    * @param source start node of the arc
    * @param destination end node the arc
    * @param weight weight of this arc for cost calculation
    * @param capacity capacity for the flow on this arc
    */
  def arc(source: Node, destination: Node, weight: IntVar, capacity: IntVar): Unit = {
    // println(source.name + " -> " + destination.name)
    addArc(nodes(source), nodes(destination), weight, capacity)
  }

  def cost(c: IntVar): Unit = setCostVariable(c)
}

object Network {
  /** Node definition for network for networkflow constraint */
  case class Node(var name: String, var balance: Int)
}