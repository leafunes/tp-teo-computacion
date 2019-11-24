package org.ungs.grammar
import org.ungs.BaseSpec


class FNCGrammarSpec extends BaseSpec{

    test("cyk test"){

        val grammar = Grammar("(,),a,b", "B,S", "S", List(
            "S->(,B,)|(,S,)|S,S",
            "B->a,B|b,B|e",
        ))

        val fnc = grammar.FNC()

        assert(fnc.CYKEfficient("(ababbabbabbababba)((abbaa))()".toCharArray().toList.map(x => Terminal(x.toString()))))

    }

    test("cyk not contains test"){

        val grammar = Grammar("(,),a,b", "B,S", "S", List(
            "S->(,B,)|(,S,)|S,S",
            "B->a,B|b,B|e",
        ))

        val fnc = grammar.FNC()

        assert(fnc.CYKEfficient("()(".toCharArray().toList.map(x => Terminal(x.toString()))) == false)

    }

}