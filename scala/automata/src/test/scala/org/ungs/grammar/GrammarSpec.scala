package org.ungs.grammar
import org.ungs.BaseSpec

class GrammarSpec extends BaseSpec {

    val V = Variable("V")
    val A = Variable("A")
    val B = Variable("B")
    val C = Variable("C")
    val D = Variable("D")
    val S = Variable("S")

    val b = Terminal("b")
    val c = Terminal("c")
    val d = Terminal("d")

    val setVariables = Set(A, B, C, D, S)
    val setTerminals = Set(b, c, d)

    def prodWithVars = (str: String) => Production(str, setTerminals, setVariables) 
    
    val p1 = Production("S->A,B", setTerminals, setVariables)
    val p2 = Production("A->a,B", setTerminals, setVariables)
    val p3 = Production("B->b", setTerminals, setVariables)

    test("get all symbols"){
        val prods = List(p1, p2 ,p3).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods)

        val allSymbols = grammar.allSymbols
        
        allSymbols.size should be equals 9
        allSymbols should contain allOf (A, B, c)
        allSymbols should not contain (V)

    }

    test("remove not in"){
        val prods = List(p1, p2 ,p3).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods) 
        val generator:((Grammar) => Set[Symbol]) = (x) => Set(A)

        val newGrammar = grammar.removeNotIn(generator)

        newGrammar.allSymbols should not contain (A)
        newGrammar.productions.size should be (1)

        assert(newGrammar.productions(0) == prodWithVars("B->b")(0))

        newGrammar.productions
            .filter(p => p.getAllSymbols().contains(A)).size should be (0)



    }

    test("remove not in with generator with outside symbol"){
        val prods = List(p1, p2 ,p3).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods) 
        val generator:((Grammar) => Set[Symbol]) = (x) => Set(V)
        val newGrammar = grammar.removeNotIn(generator)

        newGrammar.allSymbols should contain allElementsOf (grammar.allSymbols)
        grammar.allSymbols should contain allElementsOf (newGrammar.allSymbols)

        newGrammar.productions
            .filter(p => p.getAllSymbols().contains(B)).size should be (3)


    }

    test("remove nulleables"){
        val nulleable = prodWithVars("A->e")
        val newProd = prodWithVars("A->A,B,B")
        val prods = List(p1, p2 ,p3, nulleable, newProd).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods) 
        //A es nulleable
        val generator:((Grammar) => Set[Symbol]) = (x) => Set(A)

        val newGrammar = grammar.removeNulleables(generator)

        newGrammar.productions should not contain(nulleable(0))
        newGrammar.productions should contain(prodWithVars("S->B")(0))
        newGrammar.productions should contain(prodWithVars("S->A,B")(0))
        newGrammar.productions.size should be (grammar.productions.size + 1)
    }
    
    test("remove nulleables with generator outside symbol"){
        val nulleable = prodWithVars("A->e")
        val newProd = prodWithVars("A->A,B,B")
        val prods = List(p1, p2 ,p3, nulleable, newProd).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods) 
        //A es nulleable
        val generator:((Grammar) => Set[Symbol]) = (x) => Set(V)

        val newGrammar = grammar.removeNulleables(generator)

        newGrammar.allSymbols should contain allElementsOf (grammar.allSymbols)
        grammar.allSymbols should contain allElementsOf (newGrammar.allSymbols)
    }

    test("builder ok"){
        val prods = List(p1, p2 ,p3).flatten
        val grammar = new Grammar(setTerminals, setVariables, S, prods) 

        val buildedGrammar = Grammar("b,c,d", "A,B,C,D,S", "S", List(
            "S->A,B",
            "A->a,B",
            "B->b"
            ))
            

        grammar.allSymbols should contain allElementsOf (buildedGrammar.allSymbols)
        buildedGrammar.allSymbols should contain allElementsOf (grammar.allSymbols)

        grammar.productions should contain allElementsOf (buildedGrammar.productions)
        buildedGrammar.productions should contain allElementsOf (grammar.productions)
        
    }


}