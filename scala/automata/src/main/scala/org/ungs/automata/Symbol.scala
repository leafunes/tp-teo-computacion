package org.ungs.automata

case class Symbol(value: String){
    
    override def toString(): String = value

}

object Symbol{
    def apply(v: String): Symbol = {
        if(v != "e") new Symbol(v) else Epsilon
    }
}