import utils.Common.readLines

object RucksackReorganization {
    def main(args: Array[String]): Unit = {
        val rucksacks = readLines("rucksack-reorganization.txt")
        println(splitInHalf(rucksacks).map(findRepeatedItem).map(charValue).sum)
        println(groups(rucksacks).map(g => findRepeatedItem((findRepeatedItems((g.head, g(1))), g(2)))).map(charValue).sum)
    }

    def groups(l: List[String]): List[List[String]] =
        l.zipWithIndex.groupBy(_._2 / 3).values.map(e => e.map(_._1)).toList

    def splitInHalf(l: List[String]): List[(String, String)] =
        l.map(s => (s.substring(0, s.length / 2), s.substring(s.length / 2, s.length)))

    def findRepeatedItems(t: (String, String)): String = t._1.collect {
        case c if t._2.contains(c) => c
    }

    def findRepeatedItem(t: (String, String)): Char = findRepeatedItems(t).head

    def charValue(c: Char): Int = c.toByte.toInt match {
        case c if c >= 65 && c <= 90 => c - 64 + 26
        case _ => c - 96
    }
}
