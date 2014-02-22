package cf.algs

import scala.collection.mutable.IndexedSeq

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 21/02/2014
 */
object ShellSortApp extends App {
  //val input = "1".toCharArray
  val input = "1324".toCharArray


  import OrderedLike._
  val st = new ShellSort
  val od = implicitly[OrderedLike[Char]]
  st.sort(input)
  println(s"${input.mkString(", ")}")
}


trait InsertionSort extends Shuffle {

  def sortByOrdering[T](arr: IndexedSeq[T], lo: Int, hi: Int, interval: Int = 1)
             (implicit od: Ordering[T]): Unit = {
    require(lo <= hi && hi < arr.length && interval > 0)
    (interval to hi by interval) foreach { i =>
      var p = i
      (i - interval to lo by -1 * interval) foreach { j =>
        if (od.compare(arr(j), arr(p)) > 0) {
          swap(arr, p, j)
          p = j
        }
      }
    }
  }

  def sortByOrdering[T](arr: IndexedSeq[T])
                       (implicit od: Ordering[T]): Unit = {
    if (arr.length > 1) {
      sortByOrdering(arr, 0, arr.length - 1)
    }
  }

  def sort[T: OrderedLike](arr: IndexedSeq[T]): Unit = {
    if (arr.length > 1) {
      sort(arr, 0, arr.length - 1)
    }
  }

  def sort[T: OrderedLike](arr: IndexedSeq[T], lo: Int, hi: Int,
                           interval: Int = 1): Unit = {
    val od = implicitly[OrderedLike[T]]
    require(lo <= hi && hi < arr.length && interval > 0)
    (interval to hi by interval) foreach { i =>
      var p = i
      (i - interval to lo by -1 * interval) foreach { j =>
        if (od.compare(arr(j), arr(p)) > 0) {
          swap(arr, p, j)
          p = j
        }
      }
    }
  }
}

/*
class ShellSortByOrdering[T](implicit od: Ordering[T]) extends Shuffle {

  def sort(arr: Array[T]): Unit = {

    def largestInterval(l: Int): Int = {
      if (l * 3 + 1 > arr.length) l
      else largestInterval(l * 3 + 1)
    }

    def insertSort(interval: Int): Unit = {
      (interval to arr.length - 1 by interval) foreach { i =>
        var j = i
        (i - interval to 0 by (-1) * interval) foreach { k =>
          if (od.gt(arr(k), arr(j))) {
            swap(arr, k, j)
            j = k
          }
        }
      }
    }

    def hSort(h: Int): Unit = {
      if (h > 0) {
        insertSort(h)
        if (h > 1)
          hSort((h - 1) / 3)
      }
    }

    hSort(largestInterval(1))
  }
}
*/

class ShellSort extends InsertionSort {
  def largestInterval(arr: IndexedSeq[_], l: Int = 1): Int = {
    require(l > 0 && l < arr.length)
    if (l * 3 + 1 >= arr.length) l
    else largestInterval(arr, l * 3 + 1)
  }

  override def sort[T: OrderedLike](arr: IndexedSeq[T]): Unit = {
    def nextSortBy(interval: Int): Unit = {
      if (interval >= 1) {
        super.sort(arr, 0, arr.length - 1, interval)
        nextSortBy((interval - 1) / 3)
      }
    }
    nextSortBy(largestInterval(arr))
  }

  override def sortByOrdering[T](arr: IndexedSeq[T])(implicit od: Ordering[T])
  : Unit = {
    def nextSortBy(interval: Int): Unit = {
      if (interval >= 1) {
        super.sortByOrdering(arr, 0, arr.length - 1, interval)
        nextSortBy((interval - 1) / 3)
      }
    }
    nextSortBy(largestInterval(arr))
  }
}

