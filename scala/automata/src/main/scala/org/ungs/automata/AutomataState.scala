package org.ungs.automata

trait AutomataState{
    def name: String

    def isInitial(): Boolean
    def isFinal(): Boolean
    def isNormal(): Boolean

}

case class InitialState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isInitial() = true
    def isFinal() = false
    def isNormal() = false
}

case class FinalState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isInitial() = false
    def isFinal() = true
    def isNormal() = false
}

case class NormalState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isFinal() = false
    def isInitial() = false
    def isNormal() = true
}


object AutomataState{

    def apply(q: String):AutomataState = {
        if(q.startsWith("*"))
            return new FinalState(q.tail)
        if(q.startsWith("#"))
            return new InitialState(q.tail)
        return new NormalState(q)
    }

}