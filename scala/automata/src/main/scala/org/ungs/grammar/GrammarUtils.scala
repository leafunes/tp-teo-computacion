package org.ungs.grammar

object GrammarUtils {

    def getReachables(grammar: Grammar):Set[Symbol] = {

        def go(base: List[Symbol]): List[Symbol] = {
        val reachable: List[Symbol] = grammar.productions
            .filter(p => base.contains(p.left))
            .flatMap(_.right)
            .distinct
            .filter(v => !base.contains(v))

        if(reachable.isEmpty) base else go(reachable ::: base)
        }
        
        return go(List(grammar.init)).toSet
        
    }

    def getGenerators(grammar: Grammar):Set[Symbol] = {

        def go(base: List[Symbol]): List[Symbol] = {
        val generator: List[Symbol] = grammar.productions
            .filter(p => p.right.filterNot(x => base.contains(x)).isEmpty)
            .map(p => p.left)
            .filter(x => !base.contains(x))

        if(generator.isEmpty) base else go(generator ::: base)
        }
        
        return go(grammar.terminals.toList).toSet
        
    }

    def getNulleables(grammar: Grammar):Set[Symbol] = {
        def go(base: List[Symbol]): List[Symbol] = {
            val nulleables: List[Symbol] = grammar.productions
                .filter(p => p.right.filterNot(x => base.contains(x)).isEmpty)
                .map(p => p.left)
                .filter(x => !base.contains(x))
    
            if(nulleables.isEmpty) base else go(nulleables ::: base)
        }

        return go(grammar.productions.filter(_.isEpsilon).map(_.left)).toSet
    }

    def getUnits(grammar: Grammar):Set[(Variable, Variable)] = {

        def go(base:Set[(Variable, Variable)] ):Set[(Variable, Variable)] = {
            val units:Set[(Variable, Variable)] = grammar.productions
                .filter(p => p.isUnit())
                .flatMap(p => base.filter((t) => t._1.equals(p.left)).map(pr => (pr._1, p.getUnit().get)))
                .filter(x => !base.contains(x))
                .toSet
                
                
            if(units.isEmpty) base else go(units.union(base))
                
        }

        return go(grammar.variables.map(x => (x, x)))

    }
}