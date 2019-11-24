import org.ungs.grammar.Grammar
import org.ungs.grammar.Production
import org.ungs.grammar.GrammarUtils._

object Main extends App {


  val grammar = Grammar("(,),a,b", "B,S", "S", List(
            "S->(,B,)",
            "B->a,B|b,B|e",
        ))

  println(grammar.removeNotIn(getGenerators))

}