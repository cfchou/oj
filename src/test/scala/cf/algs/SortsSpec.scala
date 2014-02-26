package cf.algs

import org.scalatest.{WordSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import scala.collection.mutable.IndexedSeq

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 22/02/2014
 */
class SortsSpec extends WordSpec with PropertyChecks with Matchers {

  /* ======================
   * Selection
   * ======================
   */
  "In Selection" when {
    val sl = new Selection

    "select 1th of 4 2 5 7 1" should {
      "be 1" in {
        val noo = Array(4, 2, 5, 7, 1)
        val r = sl.select(noo, 1)
        assert(r === 1)
      }
    }

    "select by a random rank from a random series of Int" should {
      "return the element in question" in {
        val gen = Gen.containerOf1[Array, Int]({
          Gen.choose(Int.MinValue, Int.MaxValue)
        })
        forAll (gen) { input: Array[Int] =>
          val rank = 1 + util.Random.nextInt(input.length)
          val ans = input.sorted.apply(rank - 1)
          val r = sl.select(input, rank)
          assert((rank, r) === (rank, ans))
        }
      }
    }
  }

  /* ======================
   * QuickSort
   * ======================
   */
  "In QuickSort, " when {
    val qs = new QuickSort


    "partition Array(1)" should {
      "return 0" in {
        val noo = Array(1)
        val mid = qs.partition(noo, 0, 0)
        assert(mid === 0)
      }
    }
    "partition Array(2, 1)" should {
      "return 1 and change to Array(1, 2)" in {
        val noo = Array(2, 1)
        val mid = qs.partition(noo, 0, 1)
        assert(mid === 1)
      }
    }
    "partition Array(1, 2)" should {
      "return 0 and doesn't change Array" in {
        val noo = Array(1, 2)
        val mid = qs.partition(noo, 0, 1)
        assert(mid === 0)
      }
    }

    "partition Array(0, 0)" should {
      "return 0 and doesn't change Array" in {
        val noo = Array(0, 0)
        val mid = qs.partition(noo, 0, 1)
        assert(mid === 0)
      }
    }

    /* QuickSort.partition doesn't guarantee that everything to the left of
     * returned pivot is strictly smaller than arr(pivot), but partition here
     * would do by moving the pivot leftwards as much as it should.
     */
    def partition3Slow[T](arr: IndexedSeq[T], lo: Int, hi: Int)
                         (implicit od: OrderedLike[T]): Int = {
      val mid = (new QuickSort).partition(arr, lo, hi)
      (mid to lo by -1).takeWhile({ i => arr(mid) == arr(i) }).last
    }

    "3-way partition an array" should {
      "" in {
        val input = Array(18, 16, -10, -17)
        //val input = Array(1, 0)
        val arr = input.clone()
        val ans = partition3Slow(arr, 0, arr.length - 1)
        val mid = qs.partition3(input, 0, input.length - 1)
        assert(mid === ans)
      }
    }

    "3-way partition random non-empty arrays" should {
      "match what partition3Slow returns" in {
        // generate non-empty arrays
        val gen = Gen.containerOf1[Array, Int]({
          Gen.choose(Int.MinValue, Int.MaxValue)
        })
        forAll (gen) { input: Array[Int] =>
          if (input.length == 0) {
            println("why containerOf1 still generates empty collection?")
          } else {
            val arr = input.clone()
            //val orig = input.clone()
            val ans = partition3Slow(arr, 0, arr.length - 1)
            val mid = qs.partition3(input, 0, input.length - 1)
            //println(s"$mid, $ans -- ${orig.mkString(", ")}")
            assert(mid == ans)
          }
        }
      }
    }

    "sort Array(0, 0)" should {
      "return Array(0, 0)" in {
        val noo = Array(0, 0)
        qs.sort(noo)
        assert(noo === Array(0, 0))
      }
    }

    "Arrays of Int" should {
      "be sorted" in {
        forAll { input: Array[Int] =>
          val ans = input.sorted
          qs.sort(input)
          assert(input === ans)
        }
      }
    }

  }

  /* ======================
   * MergeSort
   * ======================
   */
  "MergeSort" when {
    val ms = new MergeSort
    "seeing a empty input" should {
      "do nothing" in {
        val noo = Array.empty[Int]
        ms.sort(noo)
        assert(noo.isEmpty)
      }
    }
    "seeing a single element" should {
      "do nothing" in {
        val noo = Array('a')
        ms.sort(noo)
        assert(noo(0) === 'a')
      }
    }
    // unit tests
    "array: a b" should {
      "become: a b" in {
        val noo = Array('a', 'b')
        ms.sort(noo)
        assert(noo === Array('a', 'b'))
      }
    }
    "array: bacfeighd" should {
      "become: abcdefghi" in {
        val noo = "bacfeighd".toCharArray
        ms.sort(noo)
        assert(noo === "abcdefghi".toArray)
      }
    }
    "array: 247959960, -189952368" should {
      "become: -1899523688, 247959960(test for handling overflow)" in {
        val noo = Array(247959960, -1899523688)
        ms.sort(noo)
        assert(noo === Array(-1899523688, 247959960))
      }
    }

    // property tests
    "arrays of Char" should {
      "be sorted" in {
        forAll { input: Array[Char] =>
          val ans = input.sorted
          ms.sort(input)
          assert(input === ans)
        }
      }
    }

    "arrays of Int" should {
      "be sorted" in {
        forAll { input: Array[Int] =>
          val ans = input.sorted
          ms.sort(input)
          assert(input === ans)
        }
      }
    }

    "arrays of Int" should {
      "be sorted(bottom up)" in {
        forAll { input: Array[Int] =>
          val ans = input.sorted
          ms.sortBottomUp(input)
          assert(input === ans)
        }
      }
    }

  }

}
