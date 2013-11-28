package de.sciss.poirot

import org.jacop.search.Indomain

/** Like `IndomainRandom` but with explicit random generator argument. */
class IndomainRandom[A <: IntVar](random: util.Random = new util.Random) extends Indomain[A] {
	def indomain(v: A): Int = {
		require(!v.singleton(), "Indomain should not be called with singleton domain")

		val dom   = v.domain
		val min   = dom.min()
		val size  = dom.getSize

		if (size == 0) return min

		var value = random.nextInt(size)

		val domainSize = dom.noIntervals()
		if (domainSize == 1)
			return value + min

    var i = 0
    while (i < domainSize) {
      val currentMin = dom.leftElement (i)
      val currentMax = dom.rightElement(i)

			if (currentMax - currentMin + 1 > value) return currentMin + value

      value -= currentMax - currentMin + 1
      i += 1
		}

    sys.error("This code should not be reached.")
	}
}