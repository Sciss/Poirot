/*
 *  BreakingNews.scala
 *  (Poirot)
 *
 *  Copyright (c) 2013-2018 Hanns Holger Rutz. All rights reserved.
 *  Code is often based on or identical to the original JaCoP Scala wrappers by
 *  Krzysztof Kuchcinski and Radoslaw Szymanek.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.poirot
package examples

import Implicits._

/** Solves a simple logic puzzle about reporters breaking news stories.
  *
  * Logic Puzzle : Breaking News.
  *
  * The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois,
  * and Perry) to different locations (Bayonne, New Hope, Port Charles,
  * and South Amboy) to cover four breaking news events (30-pound baby,
  * blimp launching, skyscraper dedication, and beached whale). Their
  * editor is trying to remember where each of the reporters is. Can
  * you match the name of each reporter with the place he or she was
  * sent, and the event that each covered?
  *
  * 1. The 30-pound baby wasn't born in South Amboy or New Hope.
  *
  * 2. Jimmy didn't go to Port Charles.
  *
  * 3. The blimp launching and the skyscraper dedication were covered, in
  * some order, by Lois and the reporter who was sent to Port Charles.
  *
  * 4. South Amboy was not the site of either the beached whale or the
  * skyscraper dedication.
  *
  * 5. Bayonne is either the place that Corey went or the place where the
  * whale was beached, or both.
  *
  * Determine: Reporter -- Location -- Story
  *
  * @author Marcin Chrapek, Miroslaw Klos, and Radoslaw Szymanek, clean up by H. H. Rutz
  */
object BreakingNews extends App with Problem {
  println("Program to solve Breaking News")

  // String arrays with reporters names.
  val ReporterName = Vec("Corey", "Jimmy", "Lous", "Perry")

  // Constant indexes to ease referring to variables denoting reporters.
  val iCorey = 1; val iJimmy = 2; val iLous = 3; /* val iPerry = 0; */ 
		
  // String arrays with locations names.
  val LocationName = Vec("Bayonne", "NewHope", "PortCharles", "SouthAmboy")
		
  // Constant indexes to ease referring to variables denoting locations.
  val iBayonne = 0; val iNewHope = 1; val iPortCharles = 2; val iSouthAmboy = 3

  // String arrays with stories names.
  val StoryName = Vec("30pound", "blimp", "skyscraper", "beached")

  // Constant indexes to ease referring to variables denoting stories.
  val i30pound = 0; val iblimp = 1; val iskyscraper = 2; val ibeached = 3

  // All variables are created with domain 1..4. Variables from
  // different arrays with the same values denote the same person.

  val reporter  = ReporterName.map(IntVar(_, 1, 4))
  val location  = LocationName.map(IntVar(_, 1, 4))
  val story     = StoryName   .map(IntVar(_, 1, 4))

  // It is not possible that one person has two names, or
  // has been in two locations.

  reporter.allDifferent()
  location.allDifferent()
  story  . allDifferent()

  // 1. The 30-pound baby wasn't born in South Amboy or New Hope.
  OR(story(i30pound) #!= location(iNewHope), story(i30pound) #!= location(iSouthAmboy))

  // not prettier:
  // (story(i30pound) #!= location(iNewHope)) | (story(i30pound) #!= location(iSouthAmboy))

  // 2. Jimmy didn't go to Port Charles.
  reporter(iJimmy) #!= location(iPortCharles)

  // 3.The blimp launching and the skyscraper dedication were
  // covered, in some order, by Lois and the reporter who was
  // sent to Port Charles.

  OR(AND(story(iblimp) #= reporter(iLous       ), story(iskyscraper) #= location(iPortCharles)),
     AND(story(iblimp) #= location(iPortCharles), story(iskyscraper) #= reporter(iLous       ))
  )

  // 4. South Amboy was not the site of either the beached whale
  // or the skyscraper dedication.
  OR(location(iSouthAmboy) #!= story(ibeached), location(iSouthAmboy) #!= story(iskyscraper))

  // 5. Bayonne is either the place that Corey went or the place
  // where the whale was beached, or both.
  OR(location(iBayonne) #= reporter(iCorey), location(iBayonne) #= story(ibeached),
    AND(story(ibeached) #= reporter(iCorey), reporter(iCorey) #= location(iBayonne)))

  val result = satisfy(search(reporter ++ location ++ story, inputOrder, indomainMin))
}
