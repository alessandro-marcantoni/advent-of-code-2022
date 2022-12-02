import utils.Common.readLines

import scala.annotation.tailrec

object CalorieCounting {
    def main(args: Array[String]): Unit = {
        val elvesCarriage = getElvesCarriage(readLines("calories-counting.txt")).map(_.sum)
        println(elvesCarriage.max)
        println(elvesCarriage.sorted.reverse.take(3).sum)
    }

    @tailrec
    def getElvesCarriage(l: List[String], acc: List[List[Int]] = List(List.empty)): List[List[Int]] = l match {
        case h :: t => getElvesCarriage(t, h match {
            case "" => List() :: acc
            case n => (n.toInt :: acc.head.reverse).reverse :: acc.tail
        })
        case _ => acc
    }
}
