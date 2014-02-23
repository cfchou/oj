package cf.algs

import collection.mutable.IndexedSeq
/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 23/02/2014
 */
class QuickSort extends Shuffle {
  // in-place quick sort
  def sort[T:OrderedLike](arr: IndexedSeq[T], lo: Int, hi: Int): Unit = {
    // Terminate if arr.length <= 1
    if (lo < hi) {
      val mid = partition(arr, lo, hi)
      sort(arr, lo, mid)
      sort(arr, mid + 1, hi)
    }
  }

  def sort[T:OrderedLike](arr: IndexedSeq[T]): Unit = {
    sort(arr, 0, arr.length - 1)
  }

  def partition[T](arr: IndexedSeq[T], lo: Int, hi: Int)
                  (implicit od: OrderedLike[T]): Int = {
    require(lo < hi)
    var l = lo + 1
    var h = hi
    /*
    while (l < h) {
      while (l < hi && od.compare(arr(l), arr(lo)) <= 0) { l = l + 1 }
      while (h > lo && od.compare(arr(h), arr(lo)) > 0) { h = h - 1 }
      if (l < h)
        swap(arr, l, h)
    }
    */
    def doWhile: Unit = {
      while (l < hi && od.compare(arr(l), arr(lo)) <= 0) { l = l + 1 }
      while (h > lo && od.compare(arr(h), arr(lo)) >= 0) { h = h - 1 }
      if (l < h) {
        swap(arr, l, h)
        doWhile
      }
    }

    doWhile
    swap(arr, lo, h)
    h
  }
}
