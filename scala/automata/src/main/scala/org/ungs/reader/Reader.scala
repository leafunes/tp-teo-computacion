package org.ungs.reader
import scala.io.Source

trait Reader{
    def getLines(): List[String]
}

class FileReader(path: String) extends Reader{

    def getLines(): List[String] = {
        Source.fromFile(path).getLines.toList
    }

}