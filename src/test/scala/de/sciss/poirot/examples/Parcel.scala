//package de.sciss.poirot
//package examples
//
//import collection.immutable.{IndexedSeq => Vec}
//
///** Solves a simple parcel shipment problem.
//  *
//  * @author Radoslaw Szymanek, clean up by H. H. Rutz
//  */
//object Parcel extends App with Problem {
//  val noCities = 10
//
//  // Specifies distance between any two cities
//  // 1000 - large value to remove possibility of self loop
//  val distance: Array[Array[Int]] = Array(
//    Array(1000, 85, 110, 94, 71, 76, 25, 56, 94, 67),
//    Array(85, 1000, 26, 70, 62, 60, 63, 62, 70, 49),
//    Array(110, 26, 1000, 71, 87, 89, 88, 87, 93, 73),
//    Array(94, 70, 71, 1000, 121, 19, 82, 106, 124, 105),
//    Array(71, 62, 87, 121, 1000, 104, 53, 24, 8, 13),
//    Array(76, 60, 89, 19, 104, 1000, 65, 89, 108, 93),
//    Array(25, 63, 88, 82, 53, 65, 1000, 30, 57, 46),
//    Array(56, 62, 87, 106, 24, 89, 30, 1000, 23, 20),
//    Array(94, 70, 93, 124, 8, 108, 57, 23, 1000, 20),
//    Array(67, 49, 73, 105, 13, 93, 46, 20, 20, 1000)
//  )
//
//  val maxLoad     = 5
//  val minLoad     = -6
//  val loadParcels = Array(0, 1, 5, -6, 4, 3, -5, 2, 1, -3)
//
//  var vars    = Vec.empty[IntVar]
//
//  // Denotes a city to go to from index city
//  val cities  = Vec.tabulate(noCities)(i => new IntVar("cities[" + i + "]", 1, noCities))
//  // Denotes a cost of traveling between index city and next city
//  val costs   = Vec.tabulate(noCities)(i => new IntVar("costs[" + i + "]", 0, 1000))
//  // Denotes load of person at each city
//  val loads   = Vec.tabulate(noCities)(i => new IntVar("nextLoad[" + i + "]", minLoad, maxLoad))
//  vars ++= costs
//
//  // Impose cuircuit constraint which makes sure
//  // that array cities is a hamiltonian circuit
//  circuit(cities: _*)
//
//  // We start our journey at first city
//  var startTown = cities(0)
//
//  // We have to check all steps of the trip to make
//  // sure we satisfy load constraints.
//  for (i <- 0 until cities.length) {
//    // Variable nextTown denotes city which is visited in next move
//    val nextTown = IntVar("nextTown[" + i + "]", 1, cities.length)
//    // This constraint defines nextTown value
//    elementAt(startTown, cities, nextTown)
//    // This constraint defines change in the load
//    // i denotes here i-th city on the road
//    elementAt(startTown, loadParcels, loads(i))
//    // This constraint computes cost.
//    // i denotes here the number of the city person travels from.
//    elementAt(cities(i), distance(i), costs(i))
//
//    // person has moved to the next town, so there is a new
//    // startTown
//    startTown = nextTown
//  }
//
//  // Constraints below make sure that at no city the load
//  // constraint is violated. Load is always between [0..15].
//  for (i <- 0 until cities.length) {
//    val tripLoads = List.tabulate(i)( j => loads(j))
//    val partialLoad = new IntVar("partialLoad[0-" + i + "]", 0, 15)
//    partialLoad #= sum(tripLoads: _*)
//  }
//
//   // Computes the travel cost.
//  val cost = sum(costs: _*)
//
//  val result = minimizeSeq(List(search(vars, maxRegret, indomainMin), search(cities, inputOrder, indomainMin)), cost)
//}
