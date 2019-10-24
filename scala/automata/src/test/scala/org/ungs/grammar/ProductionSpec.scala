package org.ungs.grammar;
import org.ungs.BaseSpec

class ProductionSpec extends BaseSpec{

    val V = Variable("V")
    val A = Variable("A")
    val b = Terminal("b")

    test("getAllSymbolsSpec"){
        val left = V
        val right = A :: b :: Epsilon :: Nil

        val prod = Production(left, right)

        prod.getAllSymbols() should be (List(V, A, b, Epsilon))

    }

    test("isUnitSpec"){
        val left = V
        val right = A :: Nil

        val prod = Production(left, right)

        assert(prod.isUnit())
    }

    test("isUnitWithTerminalSpec"){
        val left = V
        val right = b :: Nil

        val prod = Production(left, right)

        assert(prod.isUnit() == false)
    }

}