package cf.algs

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 22/02/2014
 */
/* Type-class for Char since Char doesn't subclass Ordered[Char].
 * Basically, it follows the same mechanism of Ordering[T].
 */
trait OrderedLike[T] {
  def compare(a: T, b: T): Int
}

object OrderedLike {
  /* Need to explicitly declare type to be OrderLike[Char]. Otherwise
   * type system can only infer a structural type which couldn't be looked up.
   */
  implicit val orderedChar: OrderedLike[Char] = new OrderedLike[Char] {
    override def compare(a: Char, b: Char): Int = {
      a.compareTo(b)
    }
  }

  implicit val orderedInt: OrderedLike[Int] = new OrderedLike[Int] {
    override def compare(a: Int, b: Int): Int = {
      // In case overflow happens
      val r = BigInt(a) - BigInt(b)
      if (r < 0) return -1
      else if (r == 0) return 0
      else return 1
    }
  }
}
