package org.ungs.automata

trait AutomataState{
    def name: String

    def isInitial(): Boolean
    def isFinal(): Boolean
    def isNormal(): Boolean
    def fold(other:AutomataState): AutomataState

}

case class InitialState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isInitial() = true
    def isFinal() = false
    def isNormal() = false

    def fold(other: AutomataState): AutomataState = {
        return new NormalState(this.q + other.name)
    }
}

case class FinalState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isInitial() = false
    def isFinal() = true
    def isNormal() = false

    def fold(other: AutomataState): AutomataState = {
        //se que yo sou final, no hace falta el check
        return new FinalState(this.q + other.name)
        
    }

}

case class NormalState(q: String) extends AutomataState{
    def name: String = this.q
    
    def isFinal() = false
    def isInitial() = false
    def isNormal() = true

    def fold(other: AutomataState): AutomataState = {
        if(other.isFinal()) new FinalState(this.q + other.name)
        else new NormalState(this.q + other.name)
    }
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