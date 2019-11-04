package org.ungs.grammar
import org.ungs.reader.Reader

object GrammarLoader{

    def loadGrammar(reader: Reader): Grammar  = {

        val lines = reader.getLines()

        val terminalsRaw = lines.head
        val variablesRest = lines.tail

        val variablesRaw = variablesRest.head
        val initRest = variablesRest.tail

        val initRaw = initRest.head
        val prodsRaw = initRest.tail

        return Grammar(terminalsRaw, variablesRaw, initRaw, prodsRaw)
    }

}