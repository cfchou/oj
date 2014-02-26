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
      //val mid = partition(arr, lo, hi)
      val mid = (new QuickSort).partition3(arr, lo, hi)
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

}
