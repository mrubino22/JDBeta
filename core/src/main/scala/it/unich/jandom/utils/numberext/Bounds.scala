package it.unich.jandom.utils.numberext

import spire.math.Numeric
import spire.syntax.cfor._

class Bounds(val data: Array[RationalExt]) extends AnyVal {
  def length = data.length
  def apply(i: Int) = data(i)
  def copy = new Bounds(data.clone)
  def update(i: Int, v: RationalExt) = data.update(i,v)

  def vertcat(that: Bounds) = {
    val newdata = new Array[RationalExt](data.length + that.data.length)
    Array.copy(data, 0, newdata, 0, data.length)
    Array.copy(that.data, 0, newdata, data.length, that.data.length)
    new Bounds(newdata)
  }

  def apply(slice: Seq[Int]) = {
    val newdata = new Array[RationalExt](slice.length)
    for ((idx, i) <- slice.zipWithIndex) {
      newdata(i) = data(idx)
    }
    new Bounds(newdata)
  }

  def forall(f: RationalExt => Boolean) = data.forall(f)

}

object Bounds {
  def fill(n: Int)(value: RationalExt) = new Bounds(Array.fill[RationalExt](n)(value))
  def apply(elem: RationalExt*) = new Bounds(elem.toArray)
}