package cf.hackerrank

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 19/02/2014
 */
object CountingSort extends App {
  Test.main(Array.empty)
  //Solution.main(Array.empty)

  object Solution {
    def main(args: Array[String]) {
      val input = io.Source.stdin.getLines().toArray.drop(1).map({ s =>
        val arr = s.split(" ")
        (arr(0).toInt, arr(1))
      }).unzip
      val cs = new CountingSort
      println(cs.counting(input._1.toArray).mkString(" "))
    }
  }


  object Test {
    def main(args: Array[String]) {
      //val input: (Array[String], Array[String]) = """10
      val input = """10
                    |4 that
                    |3 be
                    |0 to
                    |1 be
                    |5 question
                    |1 or
                    |2 not
                    |4 is
                    |2 to
                    |4 the""".stripMargin.lines.toArray.drop(1).map({ s =>
          val arr = s.split(" ")
          (arr(0).toInt, arr(1))
        }).unzip
      val cs = new CountingSort
      println(cs.counting(input._1.toArray).mkString(" "))
    }
  }
}


class CountingSort {
  def counting(arr: Array[Int]): Array[Int] = {
    var ret = Array.fill(100)(0)
    arr foreach { i =>
      ret(i) = ret(i) + 1
    }
    ret.scanLeft(0)({(z, n) =>
      z + n
    }).drop(1)
  }
}
