package org.ungs.grammar
import org.ungs.grammar.GrammarUtils._
import scala.annotation.tailrec


class FNCGrammar(terminals: Set[Terminal], variables: Set[Variable], init: Variable, productions: List[Production]) 
    extends Grammar(terminals, variables, init, productions){

    def CYKEfficient(str: List[Terminal]): Boolean = {

        // Recursivo a la cola :D
        @tailrec
        def go(acc: Map[(Int, Int), Set[Variable]], level: Int): Map[(Int, Int), Set[Variable]] = {
            if(level == str.length)
                return acc
            
            val toAdd:Map[(Int, Int), Set[Variable]] = List.range(0, str.length - level).map( i => {
                val j = i + level
                val value:Set[Variable] = List.range(i, j).flatMap(k => {
                    val left:Set[Variable] = acc.get((i, k)).getOrElse(Set.empty)
                    val right:Set[Variable] = acc.get((k + 1, j)).getOrElse(Set.empty)

                    val rightSides:List[List[Variable]] = left.flatMap(l => right.map(r => List(l, r))).toList

                    productions.filter(p => rightSides.contains(p.right)).map(p2 => p2.left)
                }).toSet
                ((i, j), value)
            }).toMap

            return go(acc ++ toAdd, level + 1)
                
        }

        val initialMap = str.zipWithIndex.map {case (x, i) => {
            ((i, i), productions.filter(_.isTerminal()).filter(_.getTerminal().get == x).map(_.left).toSet)
        }}.toMap
        
        return go(initialMap, 1).get((0, str.length - 1)).getOrElse(Set.empty).contains(init)

    }

    def CYK(str: List[Terminal]): Boolean = {

        def go(v: Variable, l: List[Terminal]): Boolean = {
            val prods = productions.filter(x => x.left == v)
            val lStr = l.foldRight("")((a, b) => a.value + b)

            if(l.size == 1){
                return prods.filter(_.isTerminal()).foldRight(false)((p, acc) => acc || (p.getTerminal().get == l.head))
            }
            
            //Obtiene todas las formas de dividir un string en dos
            val allCombinations = getAllCombinations(l)

            return allCombinations.foldRight(false)((x, acc) => {
                val left = x._1
                val right = x._2
                val nonTerminals = prods.filterNot(p => p.isTerminal())
                
                acc || nonTerminals.foldRight(false)((nt, acc2) => {
                    acc2 || (go(nt.getFirstResult().get, left) && go(nt.getSecondResult().get, right))
                })

            })

        }

        return go(init, str)

    }
}