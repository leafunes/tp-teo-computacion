import org.ungs.grammar.Grammar
import org.ungs.grammar.Production
import org.ungs.grammar.GrammarUtils._

object Main extends App {


  val grammar: Grammar = Grammar("a,bc,c", "A,B,C,S", "S", 
    List("S->A",
      "A->B,a|a",
      "C->c"
      )
  )

  println(grammar
    .removeNotIn(getReachables)
    .removeNotIn(getGenerators)
    )

}