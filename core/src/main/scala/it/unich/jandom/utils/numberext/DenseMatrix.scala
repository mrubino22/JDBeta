package it.unich.jandom.utils.numberext

import spire.syntax.cfor._
import spire.math.Rational
import it.unich.jandom.utils.breeze.countNonZero

final class DenseMatrix(val data: Array[Rational], val rows: Int) {
  val cols = if (data.length == 0 && rows == 0) 0 else data.length / rows

  def apply(i: Int, j: Int): Rational = data(i + j * rows)
  def update(i: Int, j: Int, v: Rational) = data.update(i + j * rows, v)
  def copy = new DenseMatrix(data.clone, rows)

  def +=(that: DenseMatrix): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that.data(i)
    }
    this
  }

  def -=(that: DenseMatrix): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that.data(i)
    }
    this
  }

  def +(that: DenseMatrix) = {
    val result = this.copy
    result += that
  }

  def -(that: DenseMatrix) = {
    val result = this.copy
    result -= that
  }

  def +=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that
    }
    this
  }

  def +(that: Rational) = {
    val result = this.copy
    result += that
  }

  def -=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that
    }
    this
  }

  def -(that: Rational) = {
    val result = this.copy
    result -= that
  }

  def *=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that
    }
    this
  }

  def *(that: Rational) = {
    val result = this.copy
    result *= that
  }

  def /=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that
    }
    this
  }

  def /(that: Rational) = {
    val result = this.copy
    result /= that
  }
  def *(that: DenseMatrix): DenseMatrix = {
    //require(this.cols == that.rows)
    var result = DenseMatrix.raw(this.rows, that.cols)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      cfor(0)(_ < that.cols, _ + 1) { (j) =>
        result(i, j) = Rational(0)
        cfor(0)(_ < cols, _ + 1) { (k) =>
          result(i, j) += this(i, k) * that(k, j)
        }
      }
    }
    result
  }

  def \(that: DenseMatrix): DenseMatrix = {
    //require(this.rows == that.rows, "Non-conformant matrix sizes")
    if (data.size == 0)
      DenseMatrix.zeros(0, 0)
    else {
      val A = data.clone
      val X = that.data.clone
      val Y = DenseMatrix.LUSolveArray(X, A, that.rows, that.cols)
      new DenseMatrix(Y, rows)
    }
  }

  def \(that: DenseVector): DenseVector = {
    val M = new DenseMatrix(that.data, that.length)
    val res = this \ M
    new DenseVector(res.data)
  }

  def t: DenseMatrix = {
    val result = new DenseMatrix(new Array[Rational](data.length), cols)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      cfor(0)(_ < cols, _ + 1) { (j) =>
        result(j, i) = this(i, j)
      }
    }
    result
  }

  def horzcat(that: DenseMatrix) = {
    val result = new DenseMatrix(new Array[Rational](this.rows * (this.cols + that.cols)), rows)
    Array.copy(this.data, 0, result.data, 0, data.length)
    Array.copy(that.data, 0, result.data, data.length, that.data.length)
    result
  }

  def vertcat(that: DenseMatrix) = {
    val result = new DenseMatrix(new Array[Rational]((this.rows + that.rows) * this.cols), this.rows + that.rows)
    cfor(0)(_ < cols, _ + 1) { (j) =>
      cfor(0)(_ < this.rows, _ + 1) { (i) =>
        result(i, j) = this(i, j)
      }
      cfor(0)(_ < that.rows, _ + 1) { (i) =>
        result(i + rows, j) = that(i, j)
      }
    }
    result
  }

  def row(i: Int) = {
    val data = new Array[Rational](cols)
    cfor(0)(_ < cols, _ + 1) { (j) =>
      data(j) = this(i, j)
    }
    new DenseVector(data)
  }

  def rowUpdate(i: Int, v: DenseVector): Unit = {
    cfor(0)(_ < cols, _ + 1) { (j) =>
      this(i,j) = v(j)
    }
  }

  def col(j: Int) = {
    val data = new Array[Rational](rows)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      data(i) = this(i, j)
    }
    new DenseVector(data)
  }

  def countNonZeroInRows: Array[Int] = {
    val res = new Array[Int](rows)
    var tot = 0
    cfor(0) (_ < rows, _ +1 ) { (i) =>
      tot = 0
      cfor(0) ( _ < cols, _ + 1) { (j) =>
        if (this(i,j) != 0) tot += 1
      }
      res(i) = tot
    }
    res
  }

  def apply(slicer: Seq[Int], slicec: Seq[Int]): DenseMatrix = {
    val result = new DenseMatrix(new Array[Rational](slicer.length*slicec.length), slicer.length)
    for ((i, idxi) <- slicer.zipWithIndex; (j, idxj) <- slicec.zipWithIndex) {
      result(idxi,idxj) = this(i, j)
    }
    result
  }

  def foreachPair(f: ((Int, Int), Rational) => Unit): Unit = {
    cfor (0)(_ < rows, _ +1) { (i) =>
      cfor(0) ( _ < cols, _ +1 ) { (j) =>
        f((i,j), this(i,j))
      }
    }
  }

  override def toString = {
    val sb = new StringBuilder
    cfor (0)(_ < rows, _ + 1) { (i) =>
      cfor (0)(_ < cols, _ + 1) { (j) =>
        sb ++= this(i,j).toString
        sb += ' '
      }
      sb += '\n'
    }
    sb.toString
  }
}

object DenseMatrix {

  def raw(n: Int, m: Int) = new DenseMatrix(new Array[Rational](n * m), n)

  def eye(n: Int) = {
    val data = new Array[Rational](n * n)
    cfor(0)(_ < n, _ + 1) { (i) =>
      cfor(0)(_ < n, _ + 1) { (j) =>
        data(i + j * n) = if (i == j) Rational.one else Rational.zero
      }
    }
    new DenseMatrix(data, n)
  }

  def zeros(n: Int, m: Int) = {
    val data = Array.fill[Rational](m * n)(Rational.zero)
    new DenseMatrix(data, n)
  }

  def apply(rows: DenseVector*) = {
    if (rows.isEmpty)
      new DenseMatrix(new Array[Rational](0), 0)
    else {
      val cols = rows(0).length
      val result = new DenseMatrix(new Array[Rational](cols * rows.length), rows.length)
      for ((row, i) <- rows.zipWithIndex) {
        cfor(0)(_ < cols, _ + 1) { (j) =>
          result(i, j) = row(j)
        }
      }
      result
    }
  }

  private def LUSolveArray(X: Array[Rational], A: Array[Rational], Xrows: Int, Xcols: Int): Array[Rational] = {
    val perm = (0 until Xrows).toArray
    for (i <- 0 until Xrows - 1) {
      val optPivot = (i until Xrows) find { p => !A(perm(p) + i * Xrows).isZero }
      val pivotRow = optPivot.getOrElse(throw new IllegalArgumentException("Non invertible matrix"))
      val tmp = perm(i)
      perm(i) = perm(pivotRow)
      perm(pivotRow) = tmp
      val pivot = A(perm(i) + i * Xrows)
      for (j <- i + 1 until Xrows) {
        val coeff = A(perm(j) + i * Xrows) / pivot
        cfor(0)(_ < Xrows, _ + 1) { (k) =>
          A(perm(j) + k * Xrows) -= A(perm(i) + k * Xrows) * coeff
        }
        cfor(0)(_ < Xcols, _ + 1) { (k) =>
          X(perm(j) + k * Xrows) -= X(perm(i) + k * Xrows) * coeff
        }
      }
    }
    val X1 = new Array[Rational](Xrows * Xcols)
    for (i <- Xrows - 1 to (0, -1)) {
      cfor(0)(_ < Xcols, _ + 1) { (k) =>
        X1(i + k * Xrows) = X(perm(i) + k * Xrows)
      }
      for (j <- i + 1 until Xrows)
        cfor(0)(_ < Xcols, _ + 1) { (k) =>
          X1(i + k * Xrows) -= X1(j + k * Xrows) * A(perm(i) + j * Xrows)
        }
      cfor(0)(_ < Xcols, _ + 1) { (k) =>
        X1(i + k * Xrows) /= A(perm(i) + i * Xrows)
      }
    }
    X1
  }
}
