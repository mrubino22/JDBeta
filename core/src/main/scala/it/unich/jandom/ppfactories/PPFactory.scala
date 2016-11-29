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

package it.unich.jandom.ppfactories

/**
 * This is the trait for "per program point" factories, i.e. functions
 * which build objects parameterized by program points.
 * @tparam ProgramPoint the type of program point
 * @tapram T the type of values returned by the factory
 * @author Gianluca Amato <gamato@unich.it>
 */
trait PPFactory[-ProgramPoint, +T] extends Function1[ProgramPoint, T]


object PPFactory {
  /**
   * A "per program point" implicit factory which always returns the same value.
   * @tparam T the type of the object built by the factory
   * @param obj the object returned by the factory
   * @author Gianluca Amato <gamato@unich.it>
   */
  implicit class ConstantFactory[T](private val obj: T) extends PPFactory[Any, T] {
    def apply(pp: Any) = obj
  }
}
