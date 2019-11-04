package org.ungs.grammar

import org.ungs.BaseSpec
import org.ungs.reader.Reader

class FakeReader(lines: String*) extends Reader{
    def getLines(): List[String] = lines.toList
}

class GrammarLoaderSpec extends BaseSpec {

    test("load simple grammar"){
        val grammar = Grammar("a,b", "A,B,S", "S", List(
            "S -> A",
            "S -> a",
            "A -> B,b",
            "B -> B"
        ))

        val fakeReader = new FakeReader("a,b", "A,B,S", "S","S -> A",
            "S -> a",
            "A -> B,b",
            "B -> B")
        
        val loadedGrammar = GrammarLoader.loadGrammar(fakeReader)

        loadedGrammar.allSymbols should contain allElementsOf(grammar.allSymbols)
        grammar.allSymbols should contain allElementsOf(loadedGrammar.allSymbols)

    }

}