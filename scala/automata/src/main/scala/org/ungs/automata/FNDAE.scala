package org.ungs.automata

//Conjunto de estados, alfabeto de input, funcion de trancision, estado inicial, 
// estados finales
sealed class FNDAE(transitionFunction: Map[(AutomataState, Symbol), Set[AutomataState]]) {

    def initialState:AutomataState = transitionFunction.find(x => x._1._1.isInitial()).get._1._1
    def function = transitionFunction

    def isAccepted(str: List[Symbol]): Boolean = {
        
        def go(q: AutomataState, s: List[Symbol]): Boolean = {
            if(s.isEmpty){
                return q.isFinal()
            }
            val h :: t = s
            val nextStates:Set[AutomataState] = getNextStates(q, h)
            
            if(nextStates.isEmpty)
                return false
            return nextStates.foldRight(false)((q1, b) => b || go(q1, t))
        }

        return go(initialState, str)

    }

    def clausura(state: AutomataState):Set[AutomataState] = {
        
        val initial: List[AutomataState] =  transitionFunction.get((state, Epsilon))
            .getOrElse(Set.empty[AutomataState]).toList

        if(initial.isEmpty)
            return Set.empty[AutomataState]
        return (state :: initial ::: initial.flatMap(x => clausura(x).toList))
            .distinct.toSet
    }

    def getNextStates(s: AutomataState, q: Symbol): Set[AutomataState] = {
        val nextStates: List[AutomataState] = 
            transitionFunction.get((s, q)).getOrElse(Set.empty).toList ::: clausura(s).toList
        
        return nextStates.distinct.toSet
    }

}

object FNDAE{
    def apply(statesStr: String, transitionsStrs: List[String]): FNDAE = {
        //*a -> (1: a,b,c) | (2: a,b)
        //#b -> (1: a,b,c) | (2: a,b)
        //c -> (1: a,b,c) | (2: a,b)

        val states = statesStr.split("(\\s)*,(\\s)*")
            .map(s => AutomataState(s))

        val transitionsMap = transitionsStrs.flatMap(x => {
            val stateRaw :: transitionTable :: xs = x.split("(\\s)*->(\\s)*").toList
            val state = states.find(s => s.name == stateRaw).get
            transitionTable.split("(\\s)*\\|(\\s)*").map(t => {
                val withoutBrackets = t.subSequence(1, t.length() - 1).toString()
                val symbolRaw:: destinationStatesRaw :: xs = withoutBrackets.split("(\\s)*:(\\s)*").toList
                val symbol = Symbol(symbolRaw)
                val destinationStatesList = destinationStatesRaw.split("(\\s)*,(\\s)*").toList
                val destinationStates = states.filter(s => destinationStatesList.contains(s.name))
                    .distinct.toSet
                ((state, symbol), destinationStates)
            })
        }).toMap

        return new FNDAE(transitionsMap)
    }
}
