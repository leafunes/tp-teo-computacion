package org.ungs.automata
import org.ungs.BaseSpec

class RegexSpec extends BaseSpec{

    ignore("regex simple"){
        val r = Regex("abcd")

        val str = "a".toList.map(x => Symbol(x.toString()))
        val str2 = "abcd".toList.map(x => Symbol(x.toString()))

        assert(r.isAccepted(str) == false)
        assert(r.isAccepted(str2))

    }
    
    ignore("regex astersik"){
        val r = Regex("abcd").astersik()

        val str = "".toList.map(x => Symbol(x.toString()))
        val str2 = "abcd".toList.map(x => Symbol(x.toString()))
        
        assert(r.isAccepted(str))
        assert(r.isAccepted(str2))

    }

    ignore("regex concat"){
        val r = Regex("abcd").concat("1234")

        val str = "abcd1234".toList.map(x => Symbol(x.toString()))

        assert(r.isAccepted(str))

    }
    
    ignore("regex sum"){
        val r = Regex("abcd").sum("1234")

        val str = "abcd".toList.map(x => Symbol(x.toString()))
        val str2 = "1234".toList.map(x => Symbol(x.toString()))

        assert(r.isAccepted(str))
        assert(r.isAccepted(str2))

    }

    ignore("regex hard"){
        val r = ((Regex("1").sum("0")).astersik()).sum("abc")

        val str = "1010101100abc".toList.map(x => Symbol(x.toString()))
        //val str2 = "abc".toList.map(x => Symbol(x.toString()))

        assert(r.isAccepted(str) == false)
        //assert(r.isAccepted(str2))

    }

}