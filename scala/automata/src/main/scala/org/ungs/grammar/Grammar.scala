package org.ungs.grammar

case class Grammar(terminals: Set[Terminal], variables: Set[Variable], init: Variable, productions: List[Production]) {


    def FNC(): Grammar = {
        return this;
    }

    def getAllSymbols(): List[Symbol] = {

        variables.toList ::: terminals.toList
    }

    //TODO: esta bien el nombre? es REMOVE?
    def removeNotIn(setGenerator: (Grammar) => Set[Symbol]): Grammar = {
        val allSimbols: List[Symbol] = this.getAllSymbols()
        val notIn = allSimbols.filter(x => !setGenerator(this).contains(x))
        
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