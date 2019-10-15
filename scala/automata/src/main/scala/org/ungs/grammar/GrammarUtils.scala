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
            .filter(p => !p.right.filter(x => base.contains(x)).isEmpty)
            .map(p => p.left)
            .filter(x => !base.contains(x))

        if(generator.isEmpty) base else go(generator ::: base)
        }
        
        return go(grammar.terminals.toList).toSet
        
    }
}