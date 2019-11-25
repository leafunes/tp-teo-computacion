package org.ungs.automata

class FDA (states: List[AutomataState], input: Set[Symbol], transitionFunction: Map[(AutomataState, Symbol), AutomataState]){
    
    def initialState:AutomataState = transitionFunction.find(x => x._1._1.isInitial()).get._1._1

    def isAccepted(str: List[Symbol]): Boolean = {
        
        def go(q: AutomataState, s: List[Symbol]): Boolean = {
            if(s.isEmpty){
                return q.isFinal()
            }

            val h :: t = s
            // en teoria es completa la funcion, siempre tiene un elemento
            val nextState: AutomataState = transitionFunction.get((q, h)).get

            return go(nextState, t)
        }

        return go(initialState, str)

    }

    override def toString(): String = {
        val statesStr: String = s"States: ${states.foldRight("")((x, y) => y + x.toString() + " ")}"
        val inputStr: String = s"Input: ${input.foldRight("")((x, y) => y + x.toString() + " ")}"
        val transitionStr = transitionFunction.groupBy(_._1._1).map{
            case(k, m) => 
                s"${k} -> ${
                    m.map(x => (x._1._2, x._2))
                        .map(x => s"${x._1}: (${x._2})")
                        .foldRight("")((x, y) => y + x + " | ")
                    }"
            
        }.toList.foldRight("")((x, y) => y + x.toString() + "\n")

        return statesStr + "\n" + inputStr + "\n" + transitionStr

    }

}