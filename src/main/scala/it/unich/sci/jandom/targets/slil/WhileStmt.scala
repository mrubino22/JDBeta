/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free softsare: you can redistribute it and/or modify
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
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package targets.slil

import domains.NumericalProperty
import targets.linearcondition.LinearCond
import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.domains.AbstractProperty

/**
 * The class for a while statement.
 * @param condition the guard of the statement
 * @param body the body of the statement
 */
case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {
  import AnalysisPhase._

  /**
   * This variable keeps the last value for the invariant computed during the analysis.
   */
  var lastInvariant: NumericalProperty[_] = null

  /**
   * This variable keeps the last result for the analysis of the body of the While statement.
   */
  var lastBodyResult: NumericalProperty[_] = null

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property],
    phase: AnalysisPhase, ann: Annotation[Property]): Property = {
    import parameters.WideningScope._
    import parameters.NarrowingStrategy._

    params.nestingLevel +=  1
    
    // Determines widening/narrowing operators to use
    val widening = params.wideningFactory(this, 1)
    val narrowing = params.narrowingFactory(this, 1)

    // Determines initial values for the analysis, depending on the calling phase
    var (bodyResult, invariant) =
      if (lastBodyResult != null && phase != AscendingRestart)
        (lastBodyResult.asInstanceOf[Property], lastInvariant.asInstanceOf[Property])
      else
        (input.empty, input.empty)

    // Debug
    params.log("Beginning Ascending Chain\n")
    params.log(s"Starting Invariant: $invariant\n")
    params.log(s"Input: $input\n")

    // Initialization phase: compute the effect of entering the while node from the
    // outer cycle.
    if (phase != Descending) {
      params.wideningScope match {
        case Random => invariant = invariant union input
        case Output => invariant = invariant widening (input union bodyResult)
        case BackEdges => invariant = bodyResult union input
      }
    }
    
    // Debug
    params.log(s"Entering Invariant: $invariant\n")

    // Declare a variable for the loop
    var newinvariant = invariant

    // If needed, perform ascending phase. If in AscendingRestart, perform first step in AscendingRestart,
    // and later steps in Ascending
    if (phase == Ascending || phase == AscendingRestart) do {
      // Keep the current phase (either Ascending or AscendingRestart)
      var currentPhase = phase
      invariant = newinvariant
      params.wideningScope match {
        case Random =>
          bodyResult = body.analyze(condition.analyze(invariant), params, currentPhase, ann)
          newinvariant = widening(invariant, bodyResult)
        case BackEdges =>
          bodyResult = bodyResult widening body.analyze(condition.analyze(input union newinvariant), params, currentPhase, ann)
          newinvariant =  bodyResult union input
        case Output =>
          bodyResult = body.analyze(condition.analyze(newinvariant), params, currentPhase, ann)
          newinvariant = widening(invariant, input union bodyResult)
      }
      currentPhase = Ascending
      
      // Debug     
      params.log(s"Body Result: $bodyResult\n")
      params.log(s"Invariant: $newinvariant\n")
      
    } while (newinvariant > invariant)

    // Debug
    params.log(s"Final ascending invariant: $invariant\n")

    // If needed, perform descending step
    if (phase == Descending || params.narrowingStrategy == Restart || params.narrowingStrategy == Continue) {
      // Debug
      params.log("Beginning Descending Chain\n")
      val newphase = if (params.narrowingStrategy == Restart) AscendingRestart else Descending
      do {        
        invariant = newinvariant
        val bodyResult = body.analyze(condition.analyze(invariant), params, newphase, ann)
        newinvariant = narrowing(invariant, input union bodyResult)
                
        // Debug
        params.log(s"Body Result: $bodyResult\n")
        params.log(s"Invariant: $newinvariant\n")
      } while (newinvariant < invariant)
      params.log(s"Final descending invariant: $newinvariant\n")
    }
    ann((this, 1)) = invariant
    lastInvariant = invariant
    lastBodyResult = bodyResult
    if (params.allPPResult) ann((this, 2)) = condition.analyze(invariant)
    
    params.nestingLevel -=  1
    
    return condition.opposite.analyze(invariant)
  }

  override def mkString[U <: AbstractProperty](ann: Annotation[U], level: Int, ppspec: PrettyPrinterSpec): String = {
    val spaces = ppspec.indent(level)
    spaces + "while (" + condition + ")" + "" +
      (if (ann contains (this, 1)) " " + ppspec.decorator(ann(this, 1)) else "") + " {\n" +
      (if (ann contains (this, 2)) ppspec.indent(level + 1) + ppspec.decorator(ann(this, 2)) + "\n" else "") +
      body.mkString(ann, level + 1, ppspec) + '\n' +
      spaces + '}'
  }
}
