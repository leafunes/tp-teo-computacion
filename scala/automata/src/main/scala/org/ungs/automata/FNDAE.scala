package org.ungs.automata

//Conjunto de estados, alfabeto de input, funcion de trancision, estado inicial, 
// estados finales
class FNDAE(states: List[AutomataState], transitionFunction: Map[(AutomataState, Symbol), Set[AutomataState]]) {

    def initialState:AutomataState = transitionFunction.find(x => x._1._1.isInitial()).get._1._1
    def function = transitionFunction

    def isAccepted(str: List[Symbol]): Boolean = {
        
        def go(q: AutomataState, s: List[Symbol], clausured: List[AutomataState]): Boolean = {
            if(s.isEmpty){
                return !clausura(q).filter(_.isFinal()).isEmpty
            }

            val h :: t = s
            val nextStates: List[AutomataState] = transitionFunction.get((q, h)).getOrElse(Set.empty).toList
            
            val cl: Set[AutomataState] = if(clausured.contains(q) == false) clausura(q) else Set.empty[AutomataState]

            if(nextStates.isEmpty && cl.isEmpty)
                return false

            return nextStates.foldRight(false)((q1, b) => b || go(q1, t, Nil)) || 
                cl.foldRight(false)((q1, b) => b || go(q1, s, q :: clausured))
        }

        return go(initialState, str, Nil)

    }

    def clausura(state: AutomataState):Set[AutomataState] = {
        
        def go(s: AutomataState, visited: List[AutomataState]): Set[AutomataState] = {
            val initial: List[AutomataState] =  transitionFunction.get((s, Epsilon))
                .getOrElse(Set.empty[AutomataState]).toList
                .filterNot(visited.contains(_))

            if(initial.isEmpty)
                return Set.empty[AutomataState]
            else {
                val newVisited = (visited ::: initial).distinct
                return (initial ::: initial.flatMap(x => go(x, newVisited))).toSet;
            }
        }

        return (state :: go(state, state :: Nil).toList)
            .distinct.toSet
    }

    def toFDA(): FNDAE = {

        def go(toEvaluate: List[((AutomataState, Symbol), Set[AutomataState])],
            acc: Map[(AutomataState, Symbol), Set[AutomataState]]):Map[(AutomataState, Symbol), Set[AutomataState]]  = {

                if(toEvaluate.isEmpty)
                    return acc
                else
                    null

        }

        return null 

    }

}


object FNDAE{
    def apply(statesStr: String, transitionsStrs: List[String]): FNDAE = {
        //*a -> (1: a,b,c) | (2: a,b)
        //#b -> (1: a,b,c) | (2: a,b)
        //c -> (1: a,b,c) | (2: a,b)

        val states:List[AutomataState] = statesStr.split("(\\s)*,(\\s)*").toList
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

        return new FNDAE(states, transitionsMap)
    }
}


