import utils.Common.readString

import scala.annotation.tailrec

object TuningTrouble {
    def main(args: Array[String]): Unit = {
        val input = readString("tuning-trouble.txt")
        println(firstMarker(input, 4))
        println(firstMarker(input, 14))
    }

    @tailrec
    def firstMarker(s: String, length: Int, n: Int = 0): Int = s.substring(0, length) match {
        case s if s.toSet.size == length => n + length
        case _ => firstMarker(s.drop(1), length, n + 1)
    }

}
