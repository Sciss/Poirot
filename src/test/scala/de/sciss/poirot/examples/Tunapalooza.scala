package de.sciss.poirot
package examples

/** Solves a simple logic puzzle about music concert.
  *
  * Tim and Keri have a full day ahead for themselves as they plan to see
  * and hear everything at Tunapalooza '98, the annual save-the-tuna
  * benefit concert in their hometown. To cover the most ground, they
  * will have to split up.  They have arranged to meet during four rock
  * band acts (Ellyfish, Korrupt, Retread Ed and the Flat Tires, and
  * Yellow Reef) at planned rendezvous points (carnival games,
  * information booth, mosh pit, or T-shirt vendor).  Can you help match
  * each band name with the type of music they play (country, grunge,
  * reggae, or speed metal) and Tim and Kerri's prearranged meeting spot
  * while they play?
  *
  * 1. Korrupt isn't a country or grunge music band.
  *
  * 2. Tim and Kerri won't meet at the carnival games during Ellyfish's performance.
  *
  * 3. The pair won't meet at the T-shirt vendor during the reggae band's show.
  *
  * 4. Exactly two of the following three statements are true:
  * a) Ellyfish plays grunge music.
  * b) Tim and Kerri won't meet at the information booth during a performance by Retread Ed and the Flat Tires.
  * c) The two friends won't meet at the T-shirt vendor while Yellow Reef is playing.
  *
  * 5. The country and speed metal acts are, in some order, Retread Ed and the Flat Tires
  * and the act during which Tim and Kerri will meet at the mosh pit.
  *
  * 6. The reggae band is neither Korrupt nor the act during which Tim and
  * Kerri will meet at the information booth.
  *
  * Determine: Band name -- Music type -- Meeting place
  *
  * Given solution :
  *
  * 1 Ellyfish, grunge,  vendor
  * 2 Korrupt,  metal,   mosh
  * 3 Retread,  country, information
  * 4 Yellow ,  reggae,  carnival
  *
  * @author Lesniak Kamil, Harezlak Roman, Radoslaw Szymanek, clean up by H. H. Rutz
  * @version 3.0
  */
object Tunapalooza extends App with Problem {
  var vars = Vec.empty[IntVar]

  // names
  val Ellyfish = 1; val Korrupt = 2; val Retread = 3; val Yellow = 4

  // types
  val country     = IntVar("country"    , 1, 4)
  val grunge      = IntVar("grunge"     , 1, 4)
  val reggae      = IntVar("reggae"     , 1, 4)
  val metal       = IntVar("metal"      , 1, 4)
  // places
  val carnival    = IntVar("carnival"   , 1, 4)
  val information = IntVar("information", 1, 4)
  val mosh        = IntVar("mosh"       , 1, 4)
  val vendor      = IntVar("vendor"     , 1, 4)

		// arrays of variables
  val types       = Vec(country , grunge     , reggae, metal )
  val places      = Vec(carnival, information, mosh  , vendor)

  vars ++= types
  vars ++= places

   // All types and places have to be associated with different band.
   types .allDifferent()
   places.allDifferent()

  // 1. Korrupt isn't a country or grunge music band.

  AND(country #!= Korrupt, grunge #!= Korrupt)

  // 2. Tim and Kerri won't meet at the carnival games during Ellyfish's
  // performance.

  carnival #!= Ellyfish

  // 3. The pair won't meet at the T-shirt vendor during the reggae band's
  // show.

  vendor #!= reggae

  // 4. Exactly two of the following three statements are true:
  // a) Ellyfish plays grunge music.
  // b) Tim and Kerri won't meet at the information booth during a
  // performance by Retread Ed and the Flat Tires.
  // c) The two friends won't meet at the T-shirt vendor while Yellow Reef
  // is playing.

  val statement1  = BooleanVar("s1")
  val statement2  = BooleanVar("s2")
  val statement3  = BooleanVar("s3")
  val statements  = List(statement1, statement2, statement3)

  (grunge #= Ellyfish) <=> statement1
  statement2           <=> (information #!= Retread)
  statement3           <=> (vendor      #!= Yellow )

  sum(statements) #= 2

  vars ++= statements

  // 5. The country and speed metal acts are, in some order, Retread Ed
  // and the Flat Tires
  // and the act during which Tim and Kerri will meet at the mosh pit.

  OR(country #= mosh   , metal #= mosh   )
  OR(country #= Retread, metal #= Retread)
  mosh #!= Retread

  // 6. The reggae band is neither Korrupt nor the act during which Tim
  // and
  // Kerri will meet at the information booth.

  reggae #!= Korrupt
  reggae #!= information

  val result = satisfyAll(search(vars, inputOrder, indomainMin))
}