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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.numerical

import scala.util.Try
import it.unich.jandom.domains.CachedTopBottom
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg._
import it.unich.jandom.utils.numberext.ModZ

/**
 * This is the abstract domain of parallelotopes as appears in the NSAD 2012 paper. It is written
 * using the Breeze Math library. It is not safe, due to rounding problem of arithmetic.
 *
 * @author Gianluca Amato <g.amato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class ParallelotopeDomainModZ private (favorAxes: Boolean) extends NumericalDomain {

  import breeze.linalg.operators._
  /**
   * Build a non-empty parallelotope. If the parallelotope is not empty, the result is undetermined.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertiible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */

  def apply(low: DenseVector[ModZ], A: DenseMatrix[ModZ], high: DenseVector[ModZ]): Property = {
    val isEmpty = (0 until low.size) exists { i => low(i) > high(i) }
    val isEmpty2 = (0 until low.size) exists { i => low(i).isInfinite && low(i) == high(i) }
    new Property(isEmpty || isEmpty2, low, A, high)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def top(n: Int): Property = {
    val low = DenseVector.fill(n)(ModZ.NegativeInfinity)
    val high = DenseVector.fill(n)(ModZ.PositiveInfinity)
    val A = DenseMatrix.eye[ModZ](n)
    new Property(false, low, A, high)
    /* The full parallelotope of dimension 0 is not empty! */
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def bottom(n: Int): Property = {
    val low = DenseVector.fill(n)(ModZ.one)
    val high = DenseVector.fill(n)(ModZ.zero)
    val A = DenseMatrix.eye[ModZ](n)
    new Property(true, low, A, high)
  }

  /**
   * Given the box specified by `low` and `high` and the linear form `lf`, determines the pair
   * of the least and greatest value of the linear form in the box.
   * @param lf  a linear form.
   * @param low lower bound of the box.
   * @param high higher bound of the box.
   * @note `low`, `high` and `lf` should be of the same length.
   * @return the least and greatest value of `lf` in the box determine by `low` and `high`.
   */
  private def extremalsInBox(lf: DenseVector[ModZ], low: DenseVector[ModZ], high: DenseVector[ModZ]): (ModZ, ModZ) = {
    var minc = ModZ.zero
    var maxc = ModZ.zero
    for (i <- 0 until lf.length)
      if (lf(i) > ModZ.zero) {
        minc += lf(i) * low(i)
        maxc += lf(i) * high(i)
      } else if (lf(i) < ModZ.zero) {
        minc += lf(i) * high(i)
        maxc += lf(i) * low(i)
      }
    (minc, maxc)
  }

  /**
   * Given a sequence of vectors of the same length `n`, returns a sequence of `n` indexes
   * of vectors which are linearly independent. It is based on Gaussian elimination.
   * @param m a sequence of vectors, all of the same length.
   * @return a sequence of positions in m.
   */
  private def pivoting(m: IndexedSeq[DenseVector[ModZ]]): Seq[Int] = {
    val dimension = m(0).length
    var indexes = Seq[Int]()
    var pivots = Seq[(DenseVector[ModZ], Int)]()
    var i = 0
    while (indexes.length < dimension) {
      val row = m(i).copy
      for (p <- pivots) row -= p._1 * row(p._2)
      val col = (0 until row.length) find (row(_) != ModZ.zero)
      col match {
        case Some(col) =>
          row /= row(col)
          pivots = pivots :+ Tuple2(row, col)
          indexes = indexes :+ i
        case None =>
      }
      i += 1
    }
    indexes
  }

  /**
   * This is an element of the parallelotope domain.
   *
   * @constructor Builds a parallelotope
   * @param isEmpty is true if the parallelotope is empty. In this case, the other parameters are not relevant.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */
  final class Property(
    val isEmpty: Boolean,
    val low: DenseVector[ModZ],
    val A: DenseMatrix[ModZ],
    val high: DenseVector[ModZ])
    extends NumericalProperty[Property] {

    require(low.length == A.rows)
    require(low.length == A.cols)
    require(Try(A \ DenseMatrix.eye[ModZ](dimension)).isSuccess, s"The shape matrix ${A} is not invertible")
    require(normalized)

    type Domain = ParallelotopeDomainModZ

    def domain = ParallelotopeDomainModZ.this

    private def normalized: Boolean = {
      low.length == high.length && (
        (0 until low.length - 1) forall { i =>
          !low(i).isPosInfinity &&
            !high(i).isNegInfinity &&
            (low(i) <= high(i) || isEmpty)
        })
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def widening(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return that
      val thatRotated = that.rotate(A)
      val newlow = low.copy
      val newhigh = high.copy
      for (i <- 0 to dimension - 1) {
        if (thatRotated.low(i) < low(i)) newlow(i) = ModZ.NegativeInfinity
        if (thatRotated.high(i) > high(i)) newhigh(i) = ModZ.PositiveInfinity
      }
      new Property(false, newlow, A, newhigh)
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def narrowing(that: Property): Property = {
      require(dimension == that.dimension)
      if (that.isEmpty) {
        that
      } else {
        val thatRotated = that.rotate(A)
        val newlow = low.copy
        val newhigh = high.copy
        for (i <- 0 to dimension - 1) {
          if (low(i).isInfinity) newlow(i) = thatRotated.low(i) else newlow(i) = newlow(i) min thatRotated.low(i)
          if (high(i).isInfinity) newhigh(i) = thatRotated.high(i) else newhigh(i) = newhigh(i) max thatRotated.high(i)
        }
        new Property(false, newlow, A, newhigh)
      }
    }

    /**
     * @inheritdoc
     * It is equivalent to `intersectionWeak`.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def intersection(that: Property): Property = intersectionWeak(that)

    /**
     * @inheritdoc
     * The union of two parallelotopes is not a parallelotope. Moreover, there is no least
     * parallelotopes containing the union of two parallelotopes. Hence, this methods uses
     * heuristics to find a good result.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def union(that: Property): Property = {

      /*
       * A PrioritizedConstraint is a tuple `(a, m, M, p)` where `a` is a vector, `m` and `M` are
       * reals and `p` is an integer, whose intended meaning is the constraint `m <= ax <= M`
       * with a given priority `p`.
       */
      type PrioritizedConstraint = (DenseVector[ModZ], ModZ, ModZ, Int)

      /**
       * Given a linear form `v`, compute a prioritized constraint `(v,m,M,p)`. The parameter `ownedBy`
       * tells whether the line form under consideration is one of the "native" forms of this (1) or
       * that (2). This is used to refine priorities.
       */
      def priority(v: DenseVector[ModZ], ownedBy: Int = 0): PrioritizedConstraint = {
        val y1 = A.t \ v
        val (l1, u1) = domain.extremalsInBox(y1, low, high)
        val y2 = that.A.t \ v
        val (l2, u2) = domain.extremalsInBox(y2, that.low, that.high)
        val p =
          if (l1 == l2 && l2 == u1 && u1 == u2)
            0
          else if (favorAxes && countNonZero(v) == 1)
              25 //  previous value for test: 10
          else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {
            if (l1 == l2 && u1 == u2)
              10
            else if (l1 >= l2 && u1 <= u2)
              if (ownedBy == 2) 20 else 30
            else if (l2 >= l1 && u2 <= u1)
              if (ownedBy == 1) 20 else 30
            else if (l2 <= u1 && l2 >= l1 && u2 >= u1)
              30
            else if (l2 <= l1 && u2 >= l1 && u2 <= u1)
              30
            else 40
          } else if (l1 == l2 && !l1.isInfinity && !l2.isInfinity)
            50
          else if (u1 == u2 && !u1.isInfinity && !u2.isInfinity)
            50
          else if (!l1.isInfinity && !l2.isInfinity)
            60
          else if (!u1.isInfinity && !u2.isInfinity)
            60
          else 100
        (v, l1 min l2, u1 max u2, p)
      }

      /**
       * Determines whether `v1` and `v2` are linearly dependent.
       * @return `None` if `v1` and `v2` are not linearly dependent, otherwise it is
       * `Some(k)` such that `v1 = k * v2`.
       */
      def linearDep(v1: DenseVector[ModZ], v2: DenseVector[ModZ]): Option[ModZ] = {
        var i: Int = 0
        while (i < dimension && (v1(i) == ModZ.zero || v2(i) == ModZ.zero)) i += 1
        if (i == dimension)
          Some(ModZ.one)
        else if (v1 / v1(i) == v2 / v2(i))
          Some(v1(i) / v2(i))
        else
          None
      }

      /**
       * The inversion join procedure.
       * @param vi first vector
       * @param vj second vector
       * @param min1i least value of v1 in this
       * @param min2i least value of v1 in that
       * @param min1j least value of v2 in that
       * @param min2j least value of v2 in that
       * @return None if `v1` and `v2` do not form an inversion, otherwise it is `Some(v)` where `v` is the
       * new linear form computed by the inversion procedure.
       */
      def newConstraint(vi: DenseVector[ModZ], vj: DenseVector[ModZ], min1i: ModZ, min2i: ModZ, min1j: ModZ, min2j: ModZ): Option[DenseVector[ModZ]] = {
        if (min1i.isInfinity || min2i.isInfinity || min1j.isInfinity || min2j.isInfinity) return None
        if (linearDep(vi, vj).isEmpty) return None
        val (deltai, deltaj) = if (min2j - min1j >= ModZ.zero) (min1i - min2i, min2j - min1j) else (min2i - min1i, min1j - min2j)
        if (deltai * deltaj > ModZ.zero)
          Some(-vi * deltaj - vj * deltai)
        else
          None
      }

      require(dimension == that.dimension)

      // special cases
      if (isEmpty) return that
      if (that.isEmpty) return this
      if (dimension == 0) return this;

      val thisRotated = this.rotate(that.A)
      val thatRotated = that.rotate(this.A)
      val Q = scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]()

      val bulk = DenseMatrix.vertcat(this.A, that.A)
      val min1 = DenseVector.vertcat(this.low, thisRotated.low)
      val min2 = DenseVector.vertcat(thatRotated.low, that.low)
      val max1 = DenseVector.vertcat(this.high, thisRotated.high)
      val max2 = DenseVector.vertcat(thatRotated.high, that.high)

      for (i <- 0 to dimension - 1) Q += priority(this.A.t(::, i), 1)
      for (i <- 0 to dimension - 1) Q += priority(that.A.t(::, i), 2)
      for (i <- 0 to dimension - 1; j <- i + 1 to dimension - 1) {
        val v1 = bulk.t(::, i)
        val v2 = bulk.t(::, j)
        val nc1 = newConstraint(v1, v2, min1(i), min2(i), min1(j), min2(j))
        if (nc1.isDefined) Q += priority(nc1.get)
        val nc2 = newConstraint(v1, -v2, min1(i), min2(i), -max1(j), -max2(j))
        if (nc2.isDefined) Q += priority(nc2.get)
        val nc3 = newConstraint(-v1, -v2, -max1(i), -max2(i), -max1(j), -max2(j))
        if (nc3.isDefined) Q += priority(nc3.get)
        val nc4 = newConstraint(-v1, v2, -max1(i), -max2(i), min1(j), min2(j))
        if (nc4.isDefined) Q += priority(nc4.get)
      }
      val Qsorted = Q.sortBy[Int](_._4)
      val pvt = domain.pivoting(Qsorted map (_._1))

      val newA = DenseMatrix(pvt map (Qsorted(_)._1.toArray): _*)
      val newlow = DenseVector(pvt map (Qsorted(_)._2): _*)
      val newhigh = DenseVector(pvt map (Qsorted(_)._3): _*)

      new Property(false, newlow, newA, newhigh)
    }

    /**
     * This is a variant of `union` using weak join. The shape of the resulting
     * parallelotope is the same shap of `this`.
     * @param that the abstract object to be joined with `this`.
     * @note $NOTEDIMENSION
     * @return the weak union of the two abstract objects.
     */
    def unionWeak(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return that
      if (that.isEmpty) return this
      val result = that.rotate(A)
      for (i <- 0 to dimension - 1) {
        result.low(i) = result.low(i) min low(i)
        result.high(i) = result.high(i) max high(i)
      }
      new Property(false, result.low, result.A, result.high) //this is to normalize
    }

    /**
     * This is the weak intersection of two abstract objects. The shape of the resulting
     * parallelotope is the same shap of `this`.
     * @param that the abstract object to be intersected with `this`.
     * @note $NOTEDIMENSION
     * @return the intersection of the two abstract objects.
     */
    def intersectionWeak(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return this
      if (that.isEmpty) return that
      val result = that.rotate(A)
      for (i <- 0 to dimension - 1) {
        result.low(i) = result.low(i) max low(i)
        result.high(i) = result.high(i) min high(i)
      }
      if ((0 until result.low.length) exists { i => (result.low(i) > result.high(i)) })
        bottom
      else new Property(false, result.low, result.A, result.high) //this is to normalize
    }

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = linearAssignment_m(n, LinearForm(lf.coeffs map { ModZ(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws $ILLEGAL
     */
    def linearAssignment_m(n: Int, lf: LinearForm[ModZ]): Property = {
      require(n <= dimension && lf.dimension <= dimension)
      val tcoeff = lf.homcoeffs
      val known = lf.known
      if (isEmpty) return this
      val coeff = tcoeff.padTo(dimension, ModZ.zero).toArray
      if (coeff(n) != ModZ.zero) {
        // invertible assignment
        val increment = A(::, n) :* known / coeff(n)
        val newlow = low :+ increment
        val newhigh = high :+ increment
        // in the past we could use SparseVector instead of DenseVector, but this does not work anymore
        val ei = DenseVector.zeros[ModZ](dimension)
        ei(n) = ModZ.one
        val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).t) / coeff(n)
        new Property(false, newlow, newA, newhigh)
      } else {
        // non-invertible assignment
        val newP = nonDeterministicAssignment(n)
        val Aprime = newP.A.copy
        val j = ((0 to Aprime.rows - 1) find { Aprime(_, n) != ModZ.zero }).get
        for (s <- 0 to dimension - 1 if Aprime(s, n) != ModZ.zero && s != j)
          Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
        val ei = DenseVector.zeros[ModZ](dimension)
        ei(n) = ModZ.one
        Aprime(j, ::) := (ei :- DenseVector(coeff)).t
        val newlow = newP.low.copy
        val newhigh = newP.high.copy
        newlow(j) = known
        newhigh(j) = known
        new Property(false, newlow, Aprime, newhigh)
      }
    }

    private def dotprod(x: DenseVector[ModZ], y: DenseVector[ModZ], remove: Int = -1): ModZ = {
      var sum= ModZ.zero
      for (i <- 0 until x.length if i != remove if x(i) != ModZ.zero) sum = sum + x(i) * y(i)
      sum
    }

    def linearInequality(lf: LinearForm[Double]): Property = linearInequality_m( LinearForm(lf.coeffs map { ModZ(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws ILLEGAL
     */
    def linearInequality_m(lf: LinearForm[ModZ]): Property = {
      require(lf.dimension <= dimension)
      if (isEmpty) return this
      if (dimension == 0)
        if (lf.known > ModZ.zero)
          return bottom
        else
          return this

      val known = lf.known
      val coeffs = DenseVector(lf.homcoeffs.padTo(dimension, ModZ.zero): _*)
      val coeffsTransformed = A.t \ coeffs

      val removeCandidates = (0 until dimension) find { i => coeffsTransformed(i) != ModZ.zero && low(i).isInfinity && high(i).isInfinity }
      removeCandidates match {
        case None => {
          val newlow = low.copy
          val newhigh = high.copy
          val (minc, maxc) = domain.extremalsInBox(coeffsTransformed, newlow, newhigh)
          if (minc > -known) return bottom

          val lfArgmin = coeffsTransformed mapPairs { case (i, c) => if (c > ModZ.zero) low(i) else high(i) }

          val infinities = (0 until dimension) filter { i => lfArgmin(i).isInfinity && coeffsTransformed(i) != ModZ.zero }
          infinities.size match {
            case 0 =>
              for (i <- 0 until dimension) {
                if (coeffsTransformed(i) > ModZ.zero) newhigh(i) = high(i) min (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
                else if (coeffsTransformed(i) < ModZ.zero) newlow(i) = low(i) max (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
              }
            case 1 => {
              val posinf = infinities.head
              if (coeffsTransformed(posinf) < ModZ.zero)
                newlow(posinf) = low(posinf) max ((-dotprod(coeffsTransformed, lfArgmin, posinf) - known) / coeffsTransformed(posinf))
              else
                newhigh(posinf) = high(posinf) min ((-dotprod(coeffsTransformed, lfArgmin, posinf) - known) / coeffsTransformed(posinf))
            }
            case _ =>
          }
          new Property(false, newlow, A, newhigh)
        }
        case Some(chosen) => {
          // TODO: check.. I think this may generate non-invertible matrices
          val newA = A.copy
          val newhigh = high.copy
          newA(chosen, ::) := coeffs.t
          newhigh(chosen) = -known
          new Property(false, low, newA, newhigh)
        }
      }
    }

    def linearDisequality(lf: LinearForm[Double]): Property = linearDisequality_m( LinearForm(lf.coeffs map { ModZ(_) }: _*) )
    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def linearDisequality_m(lf: LinearForm[ModZ]): Property = {
      val tcoeff = lf.homcoeffs
      val known = lf.known
      if (tcoeff.forall(_ == ModZ.zero))
        if (known == ModZ.zero) bottom else this
      else {
        val row = (0 until dimension).find(A(_, ::).t == DenseVector(tcoeff: _*))
        row match {
          case None => this
          case Some(row) =>
            if (low(row) == known && high(row) == known) bottom else this
        }
      }
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def nonDeterministicAssignment(n: Int): Property = {
      require(n <= dimension)
      if (isEmpty) return this

      val unsortedCandidates = (0 until dimension) filter { i => A(i, n) != ModZ.zero && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
      if (unsortedCandidates.isEmpty) return this

      // We prever to use as a pivot a simple constraint. Therefore, we order constraints by the number of
      // non-zero coefficients.
      val countNonZeroInRows = countNonZero(A(*, ::))
      val removeCandidates = unsortedCandidates.sortBy({ i => countNonZeroInRows(i) })
      val removeCandidatesEq = removeCandidates filter { i => low(i) == high(i) }
      val removeCandidatesBounded = removeCandidates filter { i => !low(i).isInfinity && !high(i).isInfinity }

      val pivot =
        if (!removeCandidatesEq.isEmpty) removeCandidatesEq.head
        else if (!removeCandidatesBounded.isEmpty) removeCandidatesBounded.head
        else removeCandidates.head
      val rowPivot = A(pivot, ::)

      val newA = A.copy
      val newlow = low.copy
      val newhigh = high.copy

      for (i <- removeCandidates if i != pivot) {
        val value1 = rowPivot(n)
        val value2 = A(i, n)
        val rowi = A(i, ::)
        newA(i, ::) := rowPivot * value2 - rowi * value1
        val (minPivot, maxPivot) = if (A(i, n) < ModZ.zero) (high(pivot), low(pivot)) else (low(pivot), high(pivot))
        val (mini, maxi) = if (-A(pivot, n) < ModZ.zero) (high(i), low(i)) else (low(i), high(i))
        newlow(i) = minPivot * value2 - mini * value1
        newhigh(i) = maxPivot * value2 - maxi * value1
      }
      newlow(pivot) = ModZ.NegativeInfinity
      newhigh(pivot) = ModZ.PositiveInfinity
      new Property(false, newlow, newA, newhigh)
    }

    def addVariable(): Property = {
      if (isEmpty)
        ParallelotopeDomainModZ.this.bottom(A.rows + 1)
      else {
        val e = DenseMatrix.zeros[ModZ](dimension + 1, 1)
        e(dimension, 0) = ModZ.one
        val newA = DenseMatrix.horzcat(DenseMatrix.vertcat(A, DenseMatrix.zeros[ModZ](1, dimension)), e)
        val newlow = DenseVector.vertcat(low, DenseVector(ModZ.NegativeInfinity))
        val newhigh = DenseVector.vertcat(high, DenseVector(ModZ.PositiveInfinity))
        new Property(false, newlow, newA, newhigh)
      }
    }

    def constraints = {
      if (isEmpty)
        Seq(LinearForm(1))
      else {
        val set1 = for (i <- 0 until dimension; if !low(i).isInfinity) yield -LinearForm(-low(i) +: A(i, ::).t.toScalaVector: _*)
        val set2 = for (i <- 0 until dimension; if !high(i).isInfinity) yield LinearForm(-high(i) +: A(i, ::).t.toScalaVector: _*)
        (set1 ++ set2) map { _.toDouble }
      }
    }

    def isPolyhedral = true

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(n: Int): Property = {
      def rowToSeq(M: DenseMatrix[ModZ], i: Int, n: Int): Seq[ModZ] =
        for (j <- 0 until A.rows; if j != n) yield M(i, j)

      if (isEmpty)
        ParallelotopeDomainModZ.this.bottom(A.rows - 1)
      else {
        val forgot = this.nonDeterministicAssignment(n)
        val set1 = for (i <- 0 until dimension; if !forgot.low(i).isInfinity; if forgot.A(i, n) == ModZ.zero) yield -LinearForm(-forgot.low(i) +: rowToSeq(forgot.A, i, n): _*)
        val set2 = for (i <- 0 until dimension; if !forgot.high(i).isInfinity; if forgot.A(i, n) == ModZ.zero) yield LinearForm(-forgot.high(i) +: rowToSeq(forgot.A, i, n): _*)
        (set1 ++ set2).foldLeft(ParallelotopeDomainModZ.this.top(A.rows - 1)) { (p, lf) => p.linearInequality_m(lf) }
      }
    }

    /**
     * @inheritdoc
     */
    def mapVariables(rho: Seq[Int]): Property = {
      if (isEmpty)
        this
      else {
        val slice = for (i <- 0 until dimension; j = rho.indexOf(i); if j != -1) yield j
        val newA = A(slice, slice).toDenseMatrix
        val newlow = low(slice).toDenseVector
        val newhigh = high(slice).toDenseVector
        new Property(false, newlow, newA, newhigh)
      }
    }

     def linearEvaluation(lf: LinearForm[Double]): (Double, Double) = {
       val le =linearEvaluation_m(LinearForm(lf.coeffs map { ModZ(_) }: _*))
       (le._1.toDouble, le._2.toDouble)
     }

    /**
     * Compute the minimum and maximum value of a linear form in a parallelotope.
     * @todo should be generalized to linear forms over arbitrary types.
     * @return a tuple with two components: the first component is the least value, the second component is the greatest value
     * of the linear form over the box.
     */
    def linearEvaluation_m(lf: LinearForm[ModZ]): (ModZ, ModZ) = {

      val tcoeff = lf.homcoeffs
      if (isEmpty && tcoeff.exists { _ != ModZ.zero })
        (ModZ.PositiveInfinity, ModZ.NegativeInfinity)
      else if (dimension == 0)
        (lf.known, lf.known)
      else {
        val coeff = tcoeff.padTo(dimension, ModZ.zero).toArray
        val vec = DenseVector(coeff)
        val newvec = A.t \ vec
        val newlf = lf.known +: newvec.valuesIterator.toSeq
        val (min, max) = domain.extremalsInBox(newvec, low, high)
        (min + lf.known, max + lf.known)
      }
    }

    def minimize_m(lf: LinearForm[ModZ]) = linearEvaluation_m(lf)._1

    def maximize_m(lf: LinearForm[ModZ]) = linearEvaluation_m(lf)._2

    def frequency_m(lf: LinearForm[ModZ]) = {
      val (min, max) = linearEvaluation_m(lf)
      if (min == max) Some(min) else None
    }

    def minimize(lf: LinearForm[Double]) = linearEvaluation(lf)._1

    def maximize(lf: LinearForm[Double]) = linearEvaluation(lf)._2

    def frequency(lf: LinearForm[Double]) = {
      val (min, max) = linearEvaluation(lf)
      if (min == max) Some(min) else None
    }

    def dimension = A.rows

    def isTop = low.forall(_.isNegInfinity) && high.forall(_.isPosInfinity)

    def isBottom = isEmpty

    def bottom = ParallelotopeDomainModZ.this.bottom(dimension)

    def top = ParallelotopeDomainModZ.this.top(dimension)

    def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {
      case that: Property =>
        val lte = this <= that
        val gte = that <= this
        if (lte && gte)
          Some(0)
        else if (lte)
          Some(-1)
        else if (gte)
          Some(1)
        else
          None
      case _ => None
    }

    /**
     * It computes the smallest parallelotope which contains `this` and is definable
     * over a new shape matrix `Aprime`.
     * @param Aprime the new shape matrix.
     * @note `Aprime` should be an invertible matrix of the same dimension as `this`.
     * @throws IllegalArgumentException if `Aprime` is not square or has not the correct dimension.
     */
    def rotate(Aprime: DenseMatrix[ModZ]): Property = {
      require(dimension == Aprime.rows && dimension == Aprime.cols)
      if (isEmpty) return this;
      val B = Aprime * (A \ DenseMatrix.eye[ModZ](dimension))
      val newlow = DenseVector.zeros[ModZ](dimension)
      val newhigh = DenseVector.zeros[ModZ](dimension)
      B.foreachPair {
        case ((i, j), v) =>
          if (v > ModZ.zero) {
            newlow(i) += v * low(j)
            newhigh(i) += v * high(j)
          } else if (v < ModZ.zero) {
            newhigh(i) += v * low(j)
            newlow(i) += v * high(j)
          }
      }
      new Property(false, newlow, Aprime, newhigh)
    }

    def <=[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean = {
      if (isEmpty) return (true)
      if (that.isEmpty) return (false)
      if (that.isTop) return (true)
      val ptemp = this.rotate(that.A)
      (0 to ptemp.low.length - 1) forall { i => ptemp.low(i) >= that.low(i) && ptemp.high(i) <= that.high(i) }
    }

    def >=[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      that <= this

    def <[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      (this <= that) && !(this >= that)

    def >[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      (this >= that) && !(this <= that)

    def mkString(vars: Seq[String]): String = {

      /**
       * Returns a string representation of the linear form `lf`.
       */
      def lfToString(lf: DenseVector[ModZ]): String = {
        var first = true
        var s = ""

        for (index <- 0 until dimension) {
          val coeff = lf(index)
          val term = coeff match {
            case ModZ.zero => ""
            case ModZ.one => vars(index)
            //case -1 => "-" + vars(index)
            case c => c.toString + "*" + vars(index)
          }
          if (coeff != ModZ.zero) {
            if (first || coeff < ModZ.zero) {
              s += term
              first = false
            } else if (coeff != ModZ.zero)
              s += "+" + term
          }
        }
        if (s.isEmpty) "0" else s
      }

      if (isEmpty)
        "empty"
      else {
        val eqns = for (i <- 0 until dimension) yield {
          if (low(i) < high(i))
            low(i) + " <= " + lfToString(A.t(::, i)) + " <= " + high(i)
          else lfToString(A.t(::, i)) + " = " + high(i)
        }
        eqns.mkString("[ ", " , ", " ]")
      }
    }
  }

}

/**
 * Companion class for the parallelotope domain
 */
object ParallelotopeDomainModZ {
  private lazy val standard = new ParallelotopeDomainModZ(false) with CachedTopBottom
  private lazy val favoring = new ParallelotopeDomainModZ(true) with CachedTopBottom

  /**
   * Returns an abstract domain for parallelotopes.
   * @param favorAxes determines whether the heuristics built into the domain should try to keep the variability
   * along axes at the minimum. This is generally useful when the domain is one of the component of the sum
   * combinator.
   */
  def apply(favorAxes: Boolean = false) = if (favorAxes) favoring else standard
}
