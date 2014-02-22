package cf.hackerrank

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 19/02/2014
 */
object QuickSort extends App {
  Test.main(Array.empty)
  //Solution.main(Array.empty)

  object Solution {
    def main(args: Array[String]) {
      val input = io.Source.stdin.getLines().toArray.apply(1).split(" ").
        map(_.toInt)
      val qs = new QuickSort
      qs.inplaceQuickSort(input)
      println(input.mkString(", "))
    }
  }

  object Test {
    def main(args: Array[String]) {
      val raw="""7
                |5 8 1 3 7 9 2"""
      /*
      val raw="""7
          |5 4 4"""
      val raw="""7
                |5 7 7"""
      val raw="""7
                |1 2"""
      val raw="""7
                |2 1"""
                  */
      val input= raw.stripMargin.lines.toArray.apply(1).
        split(" ").map(_.toInt)
      println(input.mkString(", "))
      val qs = new QuickSort
      //qs.partition(input, 0, input.length - 1)
      qs.inplaceQuickSort(input)
      //println(input.mkString(", "))
    }
  }
}

class QuickSort {
  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }

  def partition(arr: Array[Int], lo: Int, hi: Int): Int = {
    require(lo < hi)

    def repeat(ll: Int, hh: Int): (Int, Int) = {
      var l = ll
      var h = hh
      while (l < hi && arr(l) < arr(lo)) { l = l + 1 }
      while (h > lo && arr(h) > arr(lo)) { h = h - 1 }
      if (l < h) {
        swap(arr, l, h)
        repeat(ll, hh)
      } else {
        (l, h)
      }
    }

    val (_, h) = repeat(lo + 1, hi)
    swap(arr, lo, h)
    h
  }

  def inplaceQuickSort(arr: Array[Int], lo: Int, hi: Int): Unit = {
    if (lo < hi) {
      val mid = partition(arr, lo, hi)
      inplaceQuickSort(arr, lo, mid - 1)
      inplaceQuickSort(arr, mid + 1, hi)
    }
  }

  def inplaceQuickSort(arr: Array[Int]): Unit =
    inplaceQuickSort(arr, 0, arr.length - 1)

  def shuffle(arr: Array[Int]): Unit = {
    (1 to arr.length - 2) foreach { i =>
      val j = i + util.Random.nextInt(arr.length - 1 - i)
      if (j != i) {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
      }
    }
  }
}
