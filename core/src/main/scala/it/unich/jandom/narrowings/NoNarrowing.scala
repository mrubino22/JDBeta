/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.jandom.narrowings

import it.unich.jandom.domains.AbstractProperty

/**
 * This is a narrowing which does not perform any descending step, it just returns the
 * current value.
 * @author Gianluca Amato <gamato@unich.it>
 */
object NoNarrowing extends Narrowing {
	def apply[Property <: AbstractProperty[Property]] (current: Property, next: Property) = current
}
