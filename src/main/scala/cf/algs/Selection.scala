package cf.algs

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 24/02/2014
 */
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

// linear time selection algorithm
class Selection extends Shuffle {
  def select[T:OrderedLike](arr: IndexedSeq[T], rank: Int): T = {
    select(arr, rank, 0, arr.length - 1)
  }

  // TODO, now assuming all elements are distinct
  def select[T:OrderedLike](arr: IndexedSeq[T], rank: Int, lo: Int, hi: Int)
  : T = {
    require(rank <= hi - lo + 1)
    if (hi == lo) {
      arr(hi)
    } else {
      val mid = partition(arr, lo, hi)
      val midRank = mid - lo + 1
      if (midRank == rank) {
        arr(mid)
      } else if (midRank > rank) {
        select(arr, rank, lo, mid - 1)
      } else { // midRank < rank
        select(arr, rank - midRank, mid + 1, hi)
      }
    }
  }

  /* QuickSort.partition doesn't guarantee that everything to the left of
   * returned pivot is strictly smaller than arr(pivot), but partition here
   * would do by moving the pivot leftwards as much as it should.
   */
  def partition[T](arr: IndexedSeq[T], lo: Int, hi: Int)
                  (implicit od: OrderedLike[T]): Int = {
    val mid = (new QuickSort).partition(arr, lo, hi)
    (mid to lo by -1).takeWhile({ i => arr(mid) == arr(i) }).last
  }

}
