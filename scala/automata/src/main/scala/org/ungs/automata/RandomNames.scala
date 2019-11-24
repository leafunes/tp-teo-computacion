package org.ungs.automata
import scala.util.Random

object RandomNames{
    def get(length: Int = 6) = {
        Random.alphanumeric.take(length).mkString("")
    }
}