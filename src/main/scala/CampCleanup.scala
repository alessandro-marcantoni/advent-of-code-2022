import utils.Common.readLines

object CampCleanup {
    def main(args: Array[String]): Unit = {
        val sections = pairsSections(readLines("camp-cleanup.txt"))
        println(sections.count(p => isFullyContained(p._1, p._2)))
        println(sections.count(p => overlap(p._1, p._2)))
    }

    def pairsSections(l: List[String]): List[((Int, Int), (Int, Int))] =
        l.map(_.split(",")).map(range).map(r => (r(0), r(1)))

    def range(a: Array[String]): Array[(Int, Int)] =
        a.map(_.split("-").map(_.toInt)).map(r => (r(0), r(1)))

    def isFullyContained(a: (Int, Int), b: (Int, Int)): Boolean =
        (a._1 <= b._1 && a._2 >= b._2) || (b._1 <= a._1 && b._2 >= a._2)

    def overlap(a: (Int, Int), b: (Int, Int)): Boolean =
        (a._1 max b._1) <= (a._2 min b._2)

}
