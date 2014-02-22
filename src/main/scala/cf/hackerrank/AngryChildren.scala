package cf.hackerrank

import scala.collection.generic.CanBuildFrom

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 17/02/2014
 */
object AngryChildren extends App {
  Test.main(Array.empty)
  //Solution.main(Array.empty)

  object Solution {
    def main(args: Array[String]) {
      val input: List[Int] = io.Source.stdin.getLines().toList.
        map(_.trim.toInt)
      val ac = new AngryChildren
      val ans = ac.minUnfairness(input(1), input.drop(2))
      println(ans)
    }
  }

  object Test {
    def main(args: Array[String]) {
      val data: List[String] = """7
                    |3
                    |10
                    |100
                    |300
                    |200
                    |1000
                    |20
                    |30""".stripMargin.lines.toList
      /*
      val data: List[String] = """1
                                 |1
                                 |1""".stripMargin.lines.toList
      */

      val input = data.map(_.trim.toInt)
      val ac = new AngryChildren
      val ans = ac.minUnfairness(input(1), input.drop(2))
      println(ans)
    }
  }

}

class AngryChildren {

  def minUnfairness(k: Int, pks: List[Int]): Int = {
    val sortedPks: Array[Int] = util.Random.shuffle(pks).sorted.toArray
    if (sortedPks.length <= k) {
      sortedPks(sortedPks.length - 1) - sortedPks(0)
    } else {
      var m = Int.MaxValue
      val next = k - 1
      (0 to sortedPks.length - 1 - next) foreach { i =>
        m = math.min(m, sortedPks(i + next) - sortedPks(i))
      }
      m
    }
  }

}
