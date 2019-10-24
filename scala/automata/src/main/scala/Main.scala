import org.ungs.grammar.Grammar
import org.ungs.grammar.Production
import org.ungs.grammar.GrammarUtils._

object Main extends App {


  val grammar: Grammar = Grammar(
    "a,bc,c", 
    "A,B,C,S", 
    "S", 
    List(
      "S->A,bc",
      "A->B,a|e|C",
      "C->c"
      )
  )

  println(getUnits(grammar))

}