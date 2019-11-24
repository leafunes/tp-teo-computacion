package org.ungs.automata

class Regex(states: List[AutomataState], transitionFunction: Map[(AutomataState, Symbol), Set[AutomataState]]) 
    extends FNDAE(states, transitionFunction) {
    def t = transitionFunction

    def sum(str: String): Regex = sum(Regex(str))

    def sum(other: Regex): Regex = {
        val (i, f, nt) = getCleanTransitions()
        val (io, fo, nto) = other.getCleanTransitions()

        val newInitial:AutomataState = InitialState("I" + RandomNames.get())
        val newFinal:AutomataState = FinalState("F" + RandomNames.get())
        
        val newTransitions = nt ++ nto  ++ Map(
            ((newInitial, Epsilon), Set(i, io)),
            ((f, Epsilon), nt.getOrElse((f, Epsilon), Set.empty) ++ Set(newFinal)),
            ((fo, Epsilon), nto.getOrElse((fo, Epsilon), Set.empty) ++ Set(newFinal)),
            )
        
        val newStates = i :: f :: io :: fo :: newInitial :: newFinal :: states.filterNot(x => x.isFinal() || x.isInitial())

        return new Regex(newStates, newTransitions.toMap)

    }

    def concat(str: String): Regex = concat(Regex(str))

    def concat(other: Regex): Regex = {
        val (i, f, nt) = getCleanTransitions()
        val (io, fo, nto) = other.getCleanTransitions()

        val newInitial:AutomataState = InitialState("I" + RandomNames.get())
        val newFinal:AutomataState = FinalState("F" + RandomNames.get())

        val newTransitions = nt ++ nto ++ Map(
            ((newInitial, Epsilon), Set(i)),
            ((f, Epsilon), nt.getOrElse((f, Epsilon), Set.empty) ++ Set(io)),
            ((fo, Epsilon), nto.getOrElse((fo, Epsilon), Set.empty) ++ Set(newFinal)),
            ) 
        
        val newStates = i :: f :: io :: fo :: newInitial :: newFinal :: states.filterNot(x => x.isFinal() || x.isInitial())

        return new Regex(newStates, newTransitions.toMap)
    }

    def astersik(): Regex = {
        val (i, f, nt) = getCleanTransitions()
        val newInitial:AutomataState = InitialState("I" + RandomNames.get())
        val newFinal:AutomataState = FinalState("F" + RandomNames.get())
        
        val newTransitions = nt ++ Map(
            ((newInitial, Epsilon), Set(i, newFinal)),
            ((f, Epsilon), nt.getOrElse((f, Epsilon), Set.empty) ++ Set(newFinal)),
            ((newFinal, Epsilon), Set(newInitial))
            )
        
        val newStates = i :: f :: newInitial :: newFinal :: states.filterNot(x => x.isFinal() || x.isInitial())

        return new Regex(newStates, newTransitions.toMap)

    }

    // Se asume que solo hay un estado inicial, y un estado final.
    // Es la premisa de las regex
    def getCleanTransitions(): (AutomataState, AutomataState, Map[(AutomataState, Symbol), Set[AutomataState]]) = {

            val newInitalState:AutomataState = NormalState(super.initialState.name)
            val newFinalState:AutomataState = NormalState(states.find(_.isFinal()).get.name)

            val newTransitionFunction: Map[(AutomataState, Symbol), Set[AutomataState]] = transitionFunction.map(t => {
                if(t._1._1.isInitial())((newInitalState, t._1._2), t._2) else t
            }).map(x => {
                if(!x._2.find(y => y.isFinal()).isEmpty){
                    ((x._1._1, x._1._2), x._2.filterNot(y => y.isFinal()).toSet ++ Set(newFinalState))
                }
                else x
            }).map(x => {
                if(! x._2.find(y => y.isInitial()).isEmpty){
                    ((x._1._1, x._1._2), x._2.filterNot(y => y.isInitial()).toSet ++ Set(newInitalState))
                }
                else x
            }).map(t => {
                if(t._1._1.isFinal())((newFinalState, t._1._2), t._2) else t
            });

            return (newInitalState, newFinalState, newTransitionFunction);

    }

    def getEpsilonTransition(q: AutomataState): Set[AutomataState] = {
        return this.transitionFunction.getOrElse((q, Epsilon), Set.empty)
    }

}

object Regex{
    /*
        (Regex(abcd))*
        Regex(abcd).asterisk()
        .add(Regex(abc).sum(Regex))

        Regex(a).sum(b).asterik().sum(Regex(b))
        (a+b*)+b
        
    
    */
    def apply(str: String): Regex = {

        def go(s: List[Char], reminders: List[AutomataState], acc: List[((AutomataState, Symbol), Set[AutomataState])])
            :List[((AutomataState, Symbol), Set[AutomataState])]  = {
                if(reminders.isEmpty)
                    return acc
                
                val h :: t = reminders
                val hAcc :: tAcc = acc
                val hs :: ts = s

                val prevState = hAcc._2.head

                val accNew : List[((AutomataState, Symbol), Set[AutomataState])] = ((prevState, Symbol(hs.toString())), Set(h)) :: acc

                return go(ts, t, accNew)

        }


        //lazy val counter: Iterator[Int] = Iterator.from(1)
        val initState: AutomataState = new InitialState("I" + RandomNames.get())
        val finalState: AutomataState = new FinalState("F" + RandomNames.get())

        val states: List[AutomataState] = finalState :: List.range(1, str.length()).map(x => NormalState("S" + x.toString() + RandomNames.get()))
        /*
        abc
        I -a-> S1 -b-> S2 -c-> F
          
        */

        val h :: t = states.reverse
        val hs :: ts = str.toList

        val initial: List[((AutomataState, Symbol), Set[AutomataState])] = ((initState, Symbol(hs.toString())), Set(h)) :: Nil
        val fullStates: List[((AutomataState, Symbol), Set[AutomataState])] = go(ts, t, initial);

        return new Regex(initState :: states, fullStates.toMap);

    }
}