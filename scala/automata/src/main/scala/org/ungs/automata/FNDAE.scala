package org.ungs.automata

//Conjunto de estados, alfabeto de input, funcion de trancision, estado inicial, 
// estados finales
class FNDAE(states: List[AutomataState], input: Set[Symbol], transitionFunction: Map[(AutomataState, Symbol), Set[AutomataState]]) {

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

    def toFDA(): FDA = {

        def go(toEvaluate: List[Set[AutomataState]],
            acc: Map[(Set[AutomataState], Symbol), Set[AutomataState]]):Map[(Set[AutomataState], Symbol), Set[AutomataState]]  = 
            {
            println(toEvaluate)
            if(toEvaluate.isEmpty)
                return acc
                
            val h :: t = toEvaluate
            val rowToAdd = input.map(x => 
                (
                (h, x), 
                h.flatMap(st => transitionFunction.getOrElse((st, x), Set.empty)  ++ clausura(st)).toSet
                )
                
            ).toMap

            val alreadyAdded = acc.map(x => x._2).toList
            val newToEval = (rowToAdd.map(x => x._2).toList ::: t)
                .filterNot(alreadyAdded.contains(_))
                .distinct.filterNot(_.isEmpty)

            return go(newToEval, acc ++ rowToAdd)

        }

        val initalFunction: Map[(Set[AutomataState], Symbol), Set[AutomataState]] = input.map(x => 
            (
                (Set(initialState), x),
                transitionFunction.getOrElse((initialState, x), Set.empty) ++ clausura(initialState)
            )
        ).toMap
        
        val toEval:List[Set[AutomataState]] = initalFunction.map(x => x._2).toList.filterNot(_.isEmpty)
        val trapState = AutomataState("TRAP")

        print(initalFunction)

        val newFunction = go(toEval, initalFunction).map(x => {
            val newState:AutomataState = x._1._1.foldRight(AutomataState("#"))((x1, y) => x1.fold(y))
            val newResultState:AutomataState = x._2.foldRight(AutomataState("#"))((x1, y) => x1.fold(y))
            val newKey = (newState, x._1._2)

            if(x._2.isEmpty)
                (newKey, trapState)
            else
                (newKey, newResultState)                

        }) ++ input.map(i => ((trapState, i), trapState)).toMap

        val newStates: List[AutomataState] = newFunction.map(x => x._1._1).toList.distinct

        return new FDA(newStates, input, newFunction) 

    }

    override def toString(): String = {
        val statesStr: String = s"States: ${states.foldRight("")((x, y) => y + x.toString() + " ")}"
        val inputStr: String = s"Input: ${input.foldRight("")((x, y) => y + x.toString() + " ")}"
        val transitionStr = transitionFunction.groupBy(_._1._1).map{
            case(k, m) => 
                s"${k} -> ${
                    m.map(x => (x._1._2, x._2.foldRight("")((x2, y) => y + x2.toString() + " ")))
                        .map(x => s"${x._1}: (${x._2})")
                        .foldRight("")((x, y) => y + x + " | ")
                    }"
            
        }.toList.foldRight("")((x, y) => y + x.toString() + "\n")

        return statesStr + "\n" + inputStr + "\n" + transitionStr

    }

}


object FNDAE{
    def apply(statesStr: String, inputStr: String, transitionsStrs: List[String]): FNDAE = {
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

        val input = inputStr.split("(\\s)*,(\\s)*").map(x => Symbol(x)).toSet

        return new FNDAE(states, input, transitionsMap)
    }
}


