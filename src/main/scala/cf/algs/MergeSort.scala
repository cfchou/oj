package cf.algs

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 22/02/2014
 */
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

class MergeSort {

  def sort[T](arr: IndexedSeq[T])(implicit od: OrderedLike[T], tg: ClassTag[T])
  : Unit = {
    if (arr.length > 1) {
      sort(arr, 0, arr.length - 1)
    }
  }

  def sortBottomUp[T](arr: IndexedSeq[T])
                     (implicit od: OrderedLike[T], tg: ClassTag[T]): Unit = {
    if (arr.length > 1) {
      sortBottomUp(arr, 0, arr.length - 1)
    }
  }

  def sort[T](arr: IndexedSeq[T], lo: Int, hi: Int)
             (implicit od: OrderedLike[T], tg: ClassTag[T]): Unit = {
    require(lo <= hi && hi < arr.length)
    val aux = Array.ofDim[T](arr.length)
    sortWith(arr, lo, hi, aux)
  }

  private[this] def sortWith[T: OrderedLike](arr: IndexedSeq[T], lo: Int, hi: Int,
                               aux: Array[T]): Unit = {
    // Optimization 1: cutoff to insertion sort
    if ((hi - lo) < 7) {
      val isort = new InsertionSort {}
      isort.sort(arr, lo, hi)
    } else {
      val mid = lo + (hi - lo) / 2
      sortWith(arr, lo, mid, aux)
      sortWith(arr, mid + 1, hi, aux)
      merge(arr, lo, mid, hi, aux)
    }
  }

  def sortBottomUp[T](arr: IndexedSeq[T], lo: Int, hi: Int)
             (implicit od: OrderedLike[T], tg: ClassTag[T]): Unit = {
    require(lo <= hi && hi < arr.length)
    val aux = Array.ofDim[T](arr.length)
    val len = hi - lo + 1
    var step = 1
    while (step < len) {
      for (i <- (lo until hi by 2 * step)) {
        merge(arr, i, i + step - 1, math.min(i + step + step - 1, hi),
          aux)
      }
      step = step + step
    }
  }

  def merge[T](arr: IndexedSeq[T], lo: Int, mid: Int, hi: Int,
            aux: IndexedSeq[T])(implicit od: OrderedLike[T]): Unit = {
    // It's safe(do nothing) if mid >= hi

    /* Optimization 2: skip if
     * last element of 1st subarray <= the first element of 2ed subarray
     */
    if (mid < hi && od.compare(arr(mid), arr(mid + 1)) > 0) {
      var p = lo
      var q = mid + 1
      (lo to hi) foreach { i =>
        if (p > mid) {
          aux(i) = arr(q)
          q = q + 1
        } else if (q > hi) {
          aux(i) = arr(p)
          p = p + 1
        } else {
          if (od.compare(arr(p), arr(q)) < 0) {
            aux(i) = arr(p)
            p = p + 1
          } else {
            aux(i) = arr(q)
            q = q + 1
          }
        }
      }
      (lo to hi) foreach { i =>
        arr(i) = aux(i)
      }
    }
  }
}
