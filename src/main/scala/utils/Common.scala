package utils

import scala.io.Source
import scala.util.Using

object Common {
    def readLines(fileName: String): List[String] = Using(Source.fromResource(fileName)) { source =>
        source.getLines().toList
    }.get
}
