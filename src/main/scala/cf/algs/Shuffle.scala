package cf.algs

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 21/02/2014
 */
import util.Random.nextInt
import collection.mutable.IndexedSeq

trait Shuffle {

  def swap[T](arr: IndexedSeq[T], i: Int, j: Int): Unit = {
    if (i != j) {
      val tmp = arr(i)
      arr(i) = arr(j)
      arr(j) = tmp
    }
  }

  def shuffle[T](arr: IndexedSeq[T]): Unit = {
    // Note that (0 to 0) == Range(0), NOT Range().
    if (arr.length > 1) {
      (0 to arr.length - 2) foreach ({ i =>
        swap(arr, i, i + nextInt(arr.length - 1 - i + 1))
      })
    }
  }
}
