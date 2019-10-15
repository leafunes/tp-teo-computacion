package org.ungs.grammar

case class Production(left: Variable, right: List[Symbol]){

    def getAllSymbols(): List[Symbol] = left :: right 

    override def toString() = {
        left.value + " -> " + right.foldRight("")((x, y) => y + x.value)
    }

}


//TODO: chequear integridad
object Production{

    def apply(singleProduction: String, terminals: Set[Terminal], variables: Set[Variable]): List[Production] = {

        val prodAsList: List[String] = singleProduction.split("->").toList
        val rawOptions: List[String] = prodAsList(1).split("\\|").toList

        val left: Option[Variable] = variables.find(x => x.value == prodAsList(0))

        val options: List[List[Symbol]] = rawOptions.map(x => {
            val rightAsList = x.split(",").toList
            variables.filter(x => rightAsList.contains(x.value)).toList :::
                terminals.filter(x => rightAsList.contains(x.value)).toList
        })

        return options.map(o => new Production(left.get, o))

    }

}