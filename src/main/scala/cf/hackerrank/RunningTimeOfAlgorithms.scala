package cf.hackerrank

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 18/02/2014
 */
object RunningTimeOfAlgorithms extends App {
  Test.main(Array.empty)
  //Solution.main(Array.empty)

  object Solution {
    def main(args: Array[String]) {
      val input: Array[Int] = io.Source.stdin.getLines().toArray.apply(1).
        split(" ").map(_.toInt)
      val is = new RunningTimeOfAlgorithms
      is.insertionSort(input)
      println(is.shiftsMade)
      println(input.mkString(","))
    }
  }

  object Test {
    def main(args: Array[String]) {
      val input ="""5
                 |2 1 3 1 2
                 """.stripMargin.lines.toArray.apply(1).split(" ").map(_.toInt)
      val is = new RunningTimeOfAlgorithms
      println(input.mkString(","))
      is.insertionSort(input)
      println(is.shiftsMade)
      println(input.mkString(","))
    }
  }
}

class RunningTimeOfAlgorithms {
  private var shifts = 0
  def shiftsMade = shifts
  // return #of shifts
  def insertionSort(arr: Array[Int]): Unit = {
    shifts = 0

    def insert(j: Int, i: Int): Int = {
      if (j >= 0) {
        if (arr(j) > arr(i)) {
          val tmp = arr(j)
          arr(j) = arr(i)
          arr(i) = tmp
          shifts = shifts + 1
          insert(j - 1, j)
        } else {
          insert(j - 1, i)
        }
      } else {
        i
      }
    }

    (1 to arr.length - 1) foreach { i =>
      insert(i - 1, i)
    }
  }
}
