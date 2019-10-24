package org.ungs.grammar

case class Production(left: Variable, right: List[Symbol]){

    def getAllSymbols(): List[Symbol] = left :: right 

    def isEpsilon(): Boolean = !this.right.filter(r => r == Epsilon).isEmpty

    //TODO: ver si hay una forma de hacerlo con match
    def isUnit(): Boolean = right.size == 1 && right.filter(r => r match {
        case v: Variable => false
        case _ => true
    }).isEmpty

    def getUnit(): Option[Variable] = if (!isUnit()) None else right(0) match {
        case v:Variable => Some(v)
        case _ => None 
    }

    override def toString() = {
        left.value + " -> " + right.foldLeft("")((x, y) => x + y.value)
    }

}

//TODO: chequear integridad
object Production{

    def apply(singleProduction: String, terminals: Set[Terminal], variables: Set[Variable]): List[Production] = {

        val prodAsList: List[String] = singleProduction.split("->").toList
        val rawOptions: List[String] = prodAsList(1).split("\\|").toList

        val left: Option[Variable] = variables.find(x => x.value == prodAsList(0))
        val allSymbols = variables.toList ::: terminals.toList

        val options: List[List[Symbol]] = rawOptions.map(_.split(",").toList
            .flatMap(x => {
                if(x == "e")
                    Some(Epsilon)
                else
                    allSymbols.find(y => y.value == x)
            }))

        return options.map(o => new Production(left.get, o))

    }

}