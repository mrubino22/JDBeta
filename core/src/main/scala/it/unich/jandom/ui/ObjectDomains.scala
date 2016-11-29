/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.ui
import it.unich.jandom.domains.objects.ObjectDomainFactory
import it.unich.jandom.domains.objects.PairSharingDomain
import it.unich.jandom.domains.objects.AliasingDomain

/**
 * A parameter enumeration for the object domain which are supported in Jandom.
 * @author Gianluca Amato <gamato@unich.it>
 */
object ObjectDomains extends ParameterEnumeration[ObjectDomainFactory] {
  val name = "Object Domain"
  val description = "The object domain to use for the analysis"
  val values: Seq[ParameterValue[ObjectDomainFactory]] = Seq(
		  ParameterValue(PairSharingDomain,"Pair Sharing","The pair sharing domain by Spoto and Secci"),
 		  ParameterValue(AliasingDomain,"Aliasing","A domain for aliasing")
  )
  val default = values.last
}
