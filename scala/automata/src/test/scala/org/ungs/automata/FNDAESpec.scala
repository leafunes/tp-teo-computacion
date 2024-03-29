package org.ungs.automata
import org.ungs.BaseSpec

class FNDAESpec extends BaseSpec{

    test("builder ok test"){
        val buildedAutomata = FNDAE("#a, b, *c", "0, 1",List(
            "a -> (0: b)",
            "b -> (1: c)",
            "c -> (1: c)"
        ));

        val automata = new FNDAE(List(AutomataState("#a"), AutomataState("b"), AutomataState("*c")),
        Set(Symbol("0"), Symbol("1")),
        Map[(AutomataState, Symbol), Set[AutomataState]](
            ((AutomataState("#a"), Symbol("0")), Set(AutomataState("b"))),
            ((AutomataState("b"), Symbol("1")), Set(AutomataState("*c"))),
            ((AutomataState("*c"), Symbol("1")), Set(AutomataState("*c"))),
        ))

        buildedAutomata.initialState should be (automata.initialState)
        buildedAutomata.function should be (automata.function)

    }

    test("simple string accept") {
        val buildedAutomata = FNDAE("#a, b, *c", "0, 1", List(
            "a -> (0: b)",
            "b -> (1: c)",
            "c -> (1: c)"
        ));

        val str = "011111".toList.map(x => Symbol(x.toString()))
        val str2 = "0011111".toList.map(x => Symbol(x.toString()))

        val acceepted = buildedAutomata.isAccepted(str)
        val notAccepted = buildedAutomata.isAccepted(str2)

        assert(acceepted)
        assert(notAccepted == false)

    }

    test("simple string epsilon accept") {
        val buildedAutomata = FNDAE("#a, b, *c", "0, 1", List(
            "a -> (0: b) | (e: b)",
            "b -> (1: c) | (e: a)",
            "c -> (1: c)"
        ));

        val str = "00011".toList.map(x => Symbol(x.toString()))
        val str2 = "11111".toList.map(x => Symbol(x.toString()))

        val acceepted = buildedAutomata.isAccepted(str)
        val acceepted2 = buildedAutomata.isAccepted(str2)

        assert(acceepted)
        assert(acceepted2)

    }

    test("FDA conversion"){
        val buildedAutomata = FNDAE("#a, b, *c", "0, 1", List(
            "a -> (0: b) | (e: b)",
            "b -> (1: c) | (e: a)",
            "c -> (1: c)"
        ));

        val str = "00011".toList.map(x => Symbol(x.toString()))
        val str2 = "11111".toList.map(x => Symbol(x.toString()))

        val acceepted = buildedAutomata.isAccepted(str)
        val acceepted2 = buildedAutomata.isAccepted(str2)
        val fdaAcceepted = buildedAutomata.toFDA().isAccepted(str)
        val fdaAcceepted2 = buildedAutomata.toFDA().isAccepted(str2)

        assert(acceepted)
        assert(acceepted2)
        assert(fdaAcceepted)
        assert(fdaAcceepted2)
    }

    test("FDA conversion with empty string"){
        val buildedAutomata = FNDAE("#a, b, *c", "0, 1", List(
            "a -> (0: b) | (e: c)",
            "b -> (1: c) | (e: a)",
            "c -> (1: c)"
        ));

        val str = "".toList.map(x => Symbol(x.toString()))

        val acceepted = buildedAutomata.isAccepted(str)
        val fdaAcceepted = buildedAutomata.toFDA().isAccepted(str)

        assert(acceepted)
        assert(fdaAcceepted)

    }

}