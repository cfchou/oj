package cf.algs

import org.scalatest.{WordSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 22/02/2014
 */
class SortsSpec extends WordSpec with PropertyChecks with Matchers {

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
