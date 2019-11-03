package org.ungs.grammar;
import org.ungs.BaseSpec

class ProductionSpec extends BaseSpec{

    val V = Variable("V")
    val A = Variable("A")
    val b = Terminal("b")
    val d = Terminal("d")

    test("get all symbols"){
        val left = V
        val right = A :: b :: Epsilon :: Nil

        val prod = new Production(left, right)

        prod.getAllSymbols() should be (List(V, A, b, Epsilon))

    }

    test("is unit"){
        val left = V
        val right = A :: Nil

        val prod = new Production(left, right)

        assert(prod.isUnit())
    }

    test("is unit with variable and terminal"){
        val left = V
        val right = A :: b :: Nil

        val prod = new Production(left, right)

        assert(prod.isUnit() == false)
    }

    test("is unit with terminal in right"){
        val left = V
        val right = b :: Nil

        val prod = new Production(left, right)

        assert(prod.isUnit() == false)
    }

    test("get unit in unit production"){
        val left = V
        val right = A :: Nil

        val prod = new Production(left, right)

        assert(prod.getUnit().get == A)

    }

    test("get unit in not unit production"){
        val left = V
        val right = b :: Nil

        val prod = new Production(left, right)

        assert(prod.getUnit() == None)
    }

    test("builder ok"){
        val variablesSet = Set(V, A)
        val terminalsSet = Set(b, d)

        val buildedProd = Production("V->A,b|d", terminalsSet, variablesSet)

        buildedProd.size should be equals 2
        
        assert(buildedProd(0).left == V)
        buildedProd(0).right should be (List(A, b))

    }

}