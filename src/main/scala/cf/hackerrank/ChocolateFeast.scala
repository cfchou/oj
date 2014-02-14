package cf.hackerrank

import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 14/02/2014
 */

object ChocolateFeast extends App {
  Solution.main(Array.empty)

  object Solution {
    def main(args: Array[String]) {
      val iter = Source.stdin.getLines()
      val k = if (iter.hasNext) {
        iter.next().toInt
      } else 0

      run(new ChocolateFeast, k, iter)
    }

    def run(feast: ChocolateFeast, k: Int, it: Iterator[String]): Unit = {
      if (k > 0 && it.hasNext) {
        val arr = it.next().trim.split(" ")
        println(feast.numEat(arr(0).toInt, arr(1).toInt, arr(2).toInt))
        run(feast, k, it)
      }
    }

  }


  class ChocolateFeast {

    def numEat(money: Int, price: Int, numWtoC: Int): Int = {

      def eatMore(numChoco: Int, eaten: Int): Int = {
        if (numChoco >= numWtoC) {
          val addChoco = numChoco / numWtoC
          val eatChoco = addChoco * numWtoC
          val remainChoco = numChoco - eatChoco + addChoco
          eatMore(remainChoco, eaten + eatChoco)
        } else {
          eaten + numChoco
        }
      }

      eatMore(money / price, 0)
    }

  }
}

/*
object Solution {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT.
     * Your class should be named Solution
     */
    val iter = Source.stdin.getLines()
    iter.hasNext
  }
}
*/
