package it.unich.jandom.utils.numberext

import spire.math.Rational
import spire.syntax.cfor._
import scala.language.implicitConversions

class DenseVector(val data: Array[Rational]) extends AnyVal {
  def length = data.length
  def apply(i: Int) = data(i)
  def update(i: Int, v: Rational) = data.update(i, v)
  def copy = new DenseVector(data.clone)

  def +=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that.data(i)
    }
    this
  }

  def -=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that.data(i)
    }
    this
  }

  def *=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that.data(i)
    }
    this
  }

  def /=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that.data(i)
    }
    this
  }

  def +=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that
    }
    this
  }

  def -=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that
    }
    this
  }

  def *=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that
    }
    this
  }

  def /=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that
    }
    this
  }

  def oppose() = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) = -data(i)
    }
    this
  }

  def +(that: DenseVector) = {
    val result = this.copy
    result += that
  }

  def -(that: DenseVector) = {
    val result = this.copy
    result -= that
  }

  def *(that: DenseVector) = {
    val result = this.copy
    result *= that
  }

  def /(that: DenseVector) = {
    val result = this.copy
    result /= that
  }

  def +(that: Rational) = {
    val result = this.copy
    result += that
  }

  def -(that: Rational) = {
    val result = this.copy
    result -= that
  }

  def *(that: Rational) = {
    val result = this.copy
    result *= that
  }

  def *(that: DenseMatrix) = {
    val m = new DenseMatrix(data, data.length)
    m * that
  }

  def /(that: Rational) = {
    val result = this.copy
    result /= that
  }

  def unary_- = {
    val result = this.copy
    result.oppose()
  }

  def countNonZero: Int = {
    var tot = 0
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      if (data(i) != 0) tot += 1
    }
    tot
  }

  def t: DenseMatrix = {
    new DenseMatrix(data.clone, 1)
  }

  def mapPairs(f: (Int, Rational) => RationalExt) = {
    val newdata = new Array[RationalExt](data.length)
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      newdata(i) = f(i, data(i))
    }
    new Bounds(newdata)
  }

  def toScalaVector: Vector[Rational] = Vector(data: _*)

}

object DenseVector {
  implicit def fromTuple2(x: Tuple2[Rational, Rational]) = DenseVector(x._1, x._2)
  implicit def fromTuple3(x: Tuple3[Rational, Rational, Rational]) = DenseVector(x._1, x._2, x._3)

  def apply(elem: Rational*): DenseVector = new DenseVector(elem.toArray)

  def zeros(n: Int) = {
    val data = Array.fill[Rational](n)(Rational.zero)
    new DenseVector(data)
  }
}