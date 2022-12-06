package utils

import scala.io.Source
import scala.util.Using

object Common {
    def readLines(fileName: String): List[String] = Using(Source.fromResource(fileName)) { source =>
        source.getLines().toList
    }.get

    def readString(fileName: String): String = Using(Source.fromResource(fileName)) { source =>
        source.getLines().toList.map(_ + "\n").reduce(_ + _)
    }.get
}
