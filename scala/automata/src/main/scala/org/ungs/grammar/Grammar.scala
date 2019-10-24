package org.ungs.grammar

case class Grammar(terminals: Set[Terminal], variables: Set[Variable], init: Variable, productions: List[Production]) {

    val allSymbols = variables.toList ::: terminals.toList
    
    def FNC(): Grammar = {
        return this;
    }

    //TODO: esta bien el nombre? es REMOVE?
    def removeNotIn(setGenerator: (Grammar) => Set[Symbol]): Grammar = {
        val allSimbols: List[Symbol] = this.allSymbols
        val notIn = allSymbols.filter(x => !setGenerator(this).contains(x))
        
        val newProductions = this.productions
            .filter(p => !notIn.contains(p.left))
            .filter(p => notIn.intersect(p.right).isEmpty)
        
        val newTerminals: Set[Terminal] = newProductions
            .flatMap( p => p.getAllSymbols()
                .flatMap(x => x match {
                    case t:Terminal => Some(t)
                    case _ => None
                }
            )).toSet

        val newVariables: Set[Variable] = newProductions
            .flatMap( p => p.getAllSymbols()
                .flatMap(x => x match {
                    case v:Variable => Some(v)
                    case _ => None
                }
            )).toSet
    
        return new Grammar(newTerminals, newVariables, init, newProductions)
    
        }

      def removeNulleables(nulleablesGenerator: (Grammar) => Set[Symbol]): Grammar = {
        
        val nulleables = nulleablesGenerator(this).toList
        println(nulleables)

        def go(rightSide: List[Symbol], nulleables: List[Symbol]): List[List[Symbol]] = {

            if(rightSide.isEmpty)
                return List(Nil)

            val h = rightSide.head
            val t = rightSide.tail

            if(nulleables.contains(h)){
                val without: List[List[Symbol]] = go(t, nulleables)
                val within: List[List[Symbol]] = without.map(x => h :: x)
                return within ::: without
            }else{
                return go(t, nulleables).map(x => h :: x)
            }

        }

        val newProductions: List[Production] = this.productions
            .flatMap(p => go(p.right.
                        filter(r => r != Epsilon), nulleables)
                .filterNot(x => x.isEmpty)
                .map(x => Production(p.left, x)))

        return new Grammar(terminals, variables, init, newProductions)
      }

      override def toString(): String = {
        val initialStr = s"Inital: ${this.init.value}"

        val terminalsStr = "Terminals: " + this.terminals.foldLeft("")((x, y) => x + y.value + " ")
        val variablesStr = "Variables: " + this.variables.foldLeft("")((x, y) => x + y.value + " ")

        val productionsStr = "Productions:\n" + this.productions
            .foldLeft("")((x, y) => x + y.toString + "\n")

        return initialStr + "\n" + terminalsStr + "\n" + variablesStr + "\n" + productionsStr
      }

}

//TODO: se puede tener factories en funcional?
object Grammar {

    def apply(terminals: String, variables: String, initStr: String, productions: List[String]): Grammar = {

        val terminalSet : Set[Terminal] = terminals.split(",").distinct.map(x => new Terminal(x)).toSet
        val variableSet : Set[Variable] = variables.split(",").distinct.map(x => new Variable(x)).toSet
        val init = Variable(initStr)
        val prods: List[Production] = productions.map(x => Production(x, terminalSet, variableSet)).flatten


        return new Grammar(terminalSet, variableSet, init, prods)

    }

    

}