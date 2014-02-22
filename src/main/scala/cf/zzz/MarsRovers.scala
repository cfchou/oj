package cf.zzz

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 20/02/2014
 */
object MarsRovers extends App {
  import Direction._
  import Command._

  val input = """5 5
                |1 2 N
                |LMLMLMLMM
                |3 3 E
                |MMRMMRMRRM""".stripMargin.lines.toArray
  val area = Plateau.tupled({
    val xy = input(0).split(" ").map(_.toInt)
    (xy(0), xy(1))
  })
  val numOfRovers = (input.length - 1) / 2
  (1 to numOfRovers) foreach { i =>
    val loc = input(i * 2 - 1).split(" ")
    val dir = loc(2) match {
      case "N" => N
      case "E" => E
      case "S" => S
      case "W" => W
    }
    val cmds = input(i * 2).toArray[Char].map({
      case 'L' => L
      case 'R' => R
      case 'M' => M
    })
    val r = new Rover(loc(0).toInt, loc(1).toInt, dir, area)
    cmds foreach { r.command(_) }
    println(s"${r.location}")
  }
}


object Command extends Enumeration {
  type Command = Value
  val L, R, M = Value
}
object Direction extends Enumeration {
  type Direction = Value
  val N = Value(0)
  val E = Value(1)
  val S = Value(2)
  val W = Value(3)
  val numOfDirection = maxId
}

import Command._
import Direction._
import com.sun.javaws.exceptions.InvalidArgumentException

case class Plateau(width: Int, height: Int)

class Rover(private[this] var x: Int, private[this] var y: Int,
            private[this] var face: Direction, area: Plateau) {
  def location: (Int, Int, Direction) = (x, y, face)
  def command(cmd: Command): Unit = cmd match {
    case L =>
      face = Direction((face.id + numOfDirection - 1) %
        numOfDirection)
    case R =>
      face = Direction((face.id + 1) % numOfDirection)
    case M =>
      face match {
        case N =>
          y = y + 1
          if (y > area.height)
            throw new InvalidArgumentException(Array(cmd.toString))
        case E =>
          x = x + 1
          if (x > area.width)
            throw new InvalidArgumentException(Array(cmd.toString))
        case S =>
          y = y - 1
          if (y < 0)
            throw new InvalidArgumentException(Array(cmd.toString))
        case W =>
          x = x - 1
          if (x < 0)
            throw new InvalidArgumentException(Array(cmd.toString))
      }
  }

}

