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
      // Optimisation 1: cutoff to InsertionSort for small arrays
      if (hi - lo + 1 <= 10) {
        val is = new InsertionSort {}
        is.sort(arr, lo, hi)
      } else {
        // Optimisation 2: choose Median-of-3 as the pivot
        val med = medianOf3(arr, lo, (lo + hi) / 2, hi)
        swap(arr, lo, med)
        //val mid = partition(arr, lo, hi)
        val mid = partition3(arr, lo, hi)
        sort(arr, lo, mid)
        sort(arr, mid + 1, hi)
      }
    }
  }

  def medianOf3[T](arr: IndexedSeq[T], i: Int, j: Int, k: Int)
                          (implicit od: OrderedLike[T]): Int = {
    val ai = arr(i)
    val aj = arr(j)
    val ak = arr(k)
    if (od.compare(ai, aj) <= 0) {
      if (od.compare(ak, ai) <= 0) i
      else if (od.compare(ak, aj) <= 0) k
      else j  // ak > aj >= ai

    } else {  // ai > aj
      if (od.compare(ak, aj) <= 0) j
      else if (od.compare(ak, ai) <= 0) k
      else i  // ak > ai > aj
    }
  }

  def sort[T:OrderedLike](arr: IndexedSeq[T]): Unit = {
    sort(arr, 0, arr.length - 1)
  }

  /* partition a collection with lo <= hi.
   * It's safe to partition an 1-element array.
   * The work of checking bounds is left to calling functions(e.g. sort).
   */
  def partition[T](arr: IndexedSeq[T], lo: Int, hi: Int)
                  (implicit od: OrderedLike[T]): Int = {
    require(lo <= hi)
    var l = lo + 1
    var h = hi
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

  /* 3-way partition
   * It's safe to partition an 1-element array.
   * The work of checking bounds is left to calling functions(e.g. sort).
   */
  def partition3[T](arr: IndexedSeq[T], lo: Int, hi: Int)
                   (implicit od: OrderedLike[T]): Int = {
    var le = lo
    var l = lo + 1
    var h = hi
    def doWhile: Unit = {
      while (l < hi && od.compare(arr(l), arr(le)) <= 0) {
        if (od.compare(arr(l), arr(le)) < 0) {
          swap(arr, l, le)
          le = le + 1
        }
        l = l + 1
      }
      while (h > lo && od.compare(arr(h), arr(le)) > 0) { h = h - 1 }
      if (l < h) {
        swap(arr, l, h)
        doWhile
      }
    }

    doWhile
    if (l <= hi && od.compare(arr(l), arr(le)) < 0) {
      swap(arr, l, le)
      le = le + 1
    }
    le
  }

}
