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

package it.unich.sci.jandom.domains

import org.scalatest.FunSuite
import parma_polyhedra_library.Octagonal_Shape_double

/**
 * Test suite for the PPLProperty numerical domain.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class PPLMacroPropertySuite extends FunSuite {
  val octDomain: NumericalDomain = PPLPropertyMacros[Octagonal_Shape_double]  
  val full = octDomain.full(3)
  val empty = octDomain.empty(3)
 
  test ("full should be full") { 
    expectResult ( true ) { full.isFull }
  }
  
  test ("full should not be empty") {  
    expectResult ( false ) { full.isEmpty }  
  }
  
  test ("empty should be empty") {    
    expectResult ( true ) { empty.isEmpty }
  }
  
  test ("empty should not be full") {
	expectResult ( false ) { empty.isFull }  
  }

  test ("empty should be strictly less than full") {
    expectResult ( true ) { empty < full }
    expectResult ( true ) { empty <= full }
  }

  test ("various operations") {
    val obj = full.linearAssignment(0, Array(0,0,0), 0)
    val obj2 = full.linearAssignment(1, Array(0,0,0), 0)
    val obj3 = full.linearAssignment(2, Array(0,0,0), 0)
    val obj4 = full.linearAssignment(2, Array(0,0,0), 1)
    val obj5 = obj4 union obj3
    expectResult (true) { obj5 > obj4 }
    val obj7 = obj5.linearInequality(Array(0,0,1), 1)
    expectResult (empty) { obj7 }   
    val obj8 = obj4 widening obj3
    expectResult (obj5) { obj8 }
  }
  
  test ("string conversion") {
    val obj = full.linearInequality(Array(1,1,0),1)
    val obj2 = obj.linearInequality(Array(1,0,0), 2)
    expectResult( Seq("-x >= 2", "-x - y >= 1") ) { obj2.mkString(IndexedSeq("x","y","z")).toSeq }
    expectResult( "[ -v0 >= 2 , -v0 - v1 >= 1 ]" ) { obj2.toString }       
  }
  
  test ("string conversion for high-dimensional spaces") {
    val a = Array.fill(33)(0.0)
    a(27) = 1.0
    val obj3 = octDomain.full(33).linearInequality(a,0)
    expectResult ( "[ -v27 >= 0 ]" ) { obj3.toString }
  }
  
  test ("map dimensions") {
    val obj = full.linearInequality(Array(1,1,0),1).linearInequality(Array(1,0,0), 2)
    val obj2 = full.linearInequality(Array(1,1,0),1).linearInequality(Array(0,1,0), 2)
    val obj3 = octDomain.full(2).linearInequality(Array(1,0), 2)

    expectResult( obj ) ( obj.mapDimensions(Seq(0,1,2)))
    expectResult( obj2 ) ( obj.mapDimensions(Seq(1,0,2)))
    expectResult( obj3 ) ( obj2.mapDimensions(Seq(-1,0,1)))    
  }
}