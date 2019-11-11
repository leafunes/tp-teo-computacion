package org.ungs.grammar
import org.ungs.grammar.GrammarUtils._

case class Grammar(terminals: Set[Terminal], variables: Set[Variable], init: Variable, productions: List[Production]) {

    val allSymbols = variables.toList ::: terminals.toList
    
    def FNC(): Grammar = {

        val cleaned = this.clean()
        lazy val counter: Iterator[Int] = Iterator.from(1)

        val variablesForTerminals: Map[Terminal, Variable] = cleaned.terminals
            .flatMap(t => t match {
                case tr: Terminal => Some((tr -> Variable("V" + counter.next().toString())))
                case _ => None
            }).toMap

        val productionsWithNoTerminals:List[Production] = cleaned.productions
            .filterNot(p => p.isTerminal())
            .map(p => new Production(p.left, p.right.flatMap(r => r match {
                case t:Terminal => variablesForTerminals.get(t) //Existe siempre la key
                case other => Some(other)
            }))) ::: 
            variablesForTerminals.toList.map(t => new Production(t._2, t._1 :: Nil))
    
        val newProductions = productionsWithNoTerminals.flatMap(p => p.normalize(counter)) :::
            cleaned.productions.filter(p => p.isTerminal())

        val newVariables: Set[Variable] = (variables.toList :::
            newProductions.flatMap(p => p.getAllSymbols().flatMap(s => s match {
                case v:Variable => Some(v)
                case _ => None
            }))).distinct.toSet

        return new Grammar(this.terminals, newVariables, this.init, newProductions)

    }

    def clean(): Grammar = {
        return this.removeNulleables(getNulleables)
            .removeUnits(getUnits)
            .removeNotIn(getGenerators)
            .removeNotIn(getReachables)

    }



    //TODO: esta bien el nombre? es REMOVE?
    def removeNotIn(setGenerator: (Grammar) => Set[Symbol]): Grammar = {
        val generated = setGenerator(this)
        val notIn = allSymbols.filterNot(s => generated.contains(s))

        val newProductions = this.productions
            .filterNot(p => notIn.contains(p.left))
            .filter(p => p.right.intersect(notIn).isEmpty)
        
        val newTerminals: Set[Terminal] = terminals.filterNot(x => notIn.contains(x)).toSet

        val newVariables: Set[Variable] = variables.filterNot(x => notIn.contains(x)).toSet
    
        return new Grammar(newTerminals, newVariables, init, newProductions)
    
        }

      def removeNulleables(nulleablesGenerator: (Grammar) => Set[Symbol]): Grammar = {
        
        val nulleables = nulleablesGenerator(this).toList

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

      def removeUnits(unitsGenerator: (Grammar) => Set[(Variable, Variable)]): Grammar = {
          
        val generated = unitsGenerator(this)
        val newProductions = generated.flatMap(u => {
            productions.filterNot(p => p.isUnit())
                .filter(pr => pr.left == u._2)
                .map(x => Production(u._1, x.right))
        }).toList

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
        val prods: List[Production] = productions.flatMap(x => Production(x, terminalSet, variableSet))


        return new Grammar(terminalSet, variableSet, init, prods)

    }

    

}