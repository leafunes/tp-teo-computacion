import org.ungs.grammar.Grammar
import org.ungs.grammar.Production
import org.ungs.automata.FNDAE
import org.ungs.grammar.GrammarUtils._
import org.ungs.automata.Symbol

object Main extends App {


  val buildedAutomata = FNDAE("#a, b, *c", "0, 1", List(
    "a -> (0: b) | (e: b,c)",
    "b -> (1: c) | (e: a)",
    "c -> (1: c)"
  ));
  
  println(buildedAutomata.toFDA().isAccepted("".toList.map(x => Symbol(x.toString()))))
  println(buildedAutomata.isAccepted("".toList.map(x => Symbol(x.toString()))))

}