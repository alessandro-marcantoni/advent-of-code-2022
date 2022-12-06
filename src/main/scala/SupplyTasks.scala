import utils.Common.readString

import scala.annotation.tailrec

object SupplyTasks {
    def main(args: Array[String]): Unit = {
        val input = readString("supply-tasks.txt")
        val stacks = getStacksContent(getStacks(input).map(getRowContent(_)).dropRight(1))
        val moves = getMoves(input)
        println(makeMoves(stacks, moves).map(_.head).reduce(_ + _))
        println(makeMoves(stacks, moves)(makeMoveWithMultipleCrates).map(_.head).reduce(_ + _))
    }

    case class Move(quantity: Int, from: Int, to: Int)

    def getStacks(l: String): List[String] = l.split("\n\n")(0).split("\n").toList

    def getStacksContent(l: List[List[String]]): List[List[String]] =
        List.iterate(0, l.head.length)(_ + 1).map(i => l.map(r => r(i))).map(s => s.filter(_ != " "))

    @tailrec
    def getRowContent(s: String, acc: List[String] = List()): List[String] = s.length match {
        case 0 => acc
        case _ => getRowContent(s.drop(4), acc :+ s(1).toString)
    }

    def getMoves(l: String): List[Move] =
        l.split("\n\n")(1).split("\n").toList
          .map(_.split(" ").filter(!exclude.contains(_)).map(_.toInt))
          .map(a => Move(a(0), a(1), a(2)))

    @tailrec
    implicit def makeMove(stacks: List[List[String]], move: Move): List[List[String]] = move match {
        case Move(0, _, _) => stacks
        case Move(quantity, from, to) => makeMove(stacks.zipWithIndex.collect {
            case (value, i) if i == from - 1 => value.tail
            case (value, i) if i == to - 1 => stacks(from - 1).head :: value
            case (value, _) => value
        }, Move(quantity - 1, from, to))
    }

    def makeMoveWithMultipleCrates(stacks: List[List[String]], move: Move): List[List[String]] =
        stacks.zipWithIndex.collect {
            case (value, i) if i == move.from - 1 => value.drop(move.quantity)
            case (value, i) if i == move.to - 1 => stacks(move.from - 1).take(move.quantity) ++: value
            case (value, _) => value
        }

    @tailrec
    def makeMoves(stacks: List[List[String]], moves: List[Move])
                 (implicit moveStrategy: (List[List[String]], Move) => List[List[String]]): List[List[String]] =
        moves match {
            case h :: t => makeMoves(moveStrategy(stacks, h), t)
            case _ => stacks
        }

    val exclude: List[String] = List("move", "from", "to")
}
