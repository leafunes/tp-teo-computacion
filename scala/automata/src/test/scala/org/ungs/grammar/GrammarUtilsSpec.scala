package org.ungs.grammar
import org.ungs.BaseSpec
import org.ungs.grammar.GrammarUtils._
import org.scalatest.Ignore

class GrammarUtilsSpec extends BaseSpec{

    val V = Variable("V")
    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")
    val D = Variable("D")
    val S = Variable("S")

    val aa = Terminal("a")
    val b = Terminal("b")
    val c = Terminal("c")
    val d = Terminal("d")

    test("get reachables simple"){
        val grammar = Grammar("a", "A,B,S", "S", List(
            "S->A",
            "A->a",
            "B->a"
        ))

        val reachables = getReachables(grammar)

        reachables should not contain(B)
        reachables should contain(aa)
        reachables.size should be (3)

    }

    test("get reachables should remove terminal simple"){
        val grammar = Grammar("a,b", "A,B,S", "S", List(
            "S->A",
            "A->a",
            "B->b"
        ))

        val reachables = getReachables(grammar)

        reachables should not contain(B)
        reachables should contain(aa)
        reachables should not contain(b)
        reachables.size should be (3)

    }

    test("get reachables transitivity"){
        val grammar = Grammar("a,b", "A,B,C,S", "S", List(
            "S->A",
            "A->C",
            "B->a",
            "C->b"
        ))

        val reachables = getReachables(grammar)

        reachables should not contain(B)
        reachables.size should be (4)

    }

    test("get reachables all are reachables"){
        val grammar = Grammar("a", "A,B,S", "S", List(
            "S->A",
            "A->B",
            "B->a",
        ))

        val reachables = getReachables(grammar)

        reachables.size should be (grammar.allSymbols.size)

    }

    test("get generators simple"){
        //B no es generador
        val grammar = Grammar("a", "A,B,S", "S", List(
            "S->A",
            "A->B|a",
            "B->a,B",
        ))

        val generatos = getGenerators(grammar)

        generatos should not contain(B)
        generatos should contain(aa)
        generatos.size should be(3)
    }

    test("get generators epsilon"){
        //B no es generador
        val grammar = Grammar("a", "A,B,C,S", "S", List(
            "S->A",
            "A->B|a",
            "B->a,B",
            "C->e"
        ))

        val generatos = getGenerators(grammar)

        generatos should not contain(B)
        generatos should contain(C)
        generatos.size should be(4)
    }

    test("get generators all are generators"){
        val grammar = Grammar("a", "A,S", "S", List(
            "S->A",
            "A->a",
        ))

        val generatos = getGenerators(grammar)

        generatos.size should be (grammar.allSymbols.size)

    }

    test("get nulleables simple"){
        //B es nulleable
        val grammar = Grammar("a", "A,B,S", "S", List(
            "S->A",
            "A->a",
            "B->e",
        ))

        val nulleables = getNulleables(grammar)

        nulleables should contain(B)
        nulleables.size should be(1)
    }

    test("get nulleables transitivity"){
        //A y B son nulleable
        val grammar = Grammar("a", "A,B,S,C", "S", List(
            "S->C",
            "A->a|B",
            "B->e",
        ))

        val nulleables = getNulleables(grammar)

        nulleables should contain(B)
        nulleables should contain(A)
        nulleables.size should be(2)
    }

    test("get nulleables are all nulleables"){
        //A y B son nulleable
        val grammar = Grammar("a", "A,B,S,", "S", List(
            "S->A",
            "A->a|B",
            "B->e",
        ))

        val nulleables = getNulleables(grammar)

        nulleables should contain allElementsOf(grammar.variables)
    }

    test("get unit simple"){
        val grammar = Grammar("a", "A,B,S,C", "S", List(
            "S->A,B",
            "A->a",
            "B->C",
        ))

        val units = getUnits(grammar)

        units should contain ((B,C))
        units.size should be (grammar.variables.size + 1)
    }

    test("get unit transitivity"){
        val grammar = Grammar("a", "A,B,S,C", "S", List(
            "S->A|A,a",
            "A->C",
            "C->B",
        ))

        val units = getUnits(grammar)

        units should contain allOf ((S,A), (S,C), (S,B))
    }

}