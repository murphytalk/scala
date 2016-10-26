package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val text = List('B','A','C','D','E','F','G','H','D')
    val tree = createCodeTree(("A"*8).toList ::: ("B"*3).toList ::: 
                               List('C') ::: List('D') ::: List('E') ::: 
                               List('F') ::: List('G') ::: List('H'))        
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")"){
    val l = times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
    assert(l === List(('d',1), ('l',3), ('r',1), ('o',2), ('w',1), (' ',1), (',',1), ('e',1), ('h',1)))
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of some leaf list2") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 5))
    assert(combine(leaflist) === List(Leaf('x',5),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7)))
  }
  
  test("combine of some leaf list3") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 5), Leaf('z', 50))
    assert(combine(leaflist) === List(Leaf('x',5),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7), Leaf('z', 50)))    
  }
  
  test("create code tree and encode and decode"){
    new TestTrees {
	    val encoded = encode(tree)(text)
	    val decoded = decode(tree,encoded)
	    println("Encoded as "+encoded)
	    println("Decoded as "+decoded)
	    assert(text === decoded)
    }
  }

  test("quickencode and decode"){
    new TestTrees {
	    
	    val encoded = quickEncode(tree)(text)
	    val decoded = decode(tree,encoded)
	    println("Quick encoded as "+encoded)
	    println("Decoded as "+decoded)
	    assert(text === decoded)
    }
  }  
   
  
  test("encode and decode using french code"){
    val huffman = List('h', 'u', 'f', 'f', 'm', 'a','n','e', 's', 't', 'c', 'o', 'o', 'l')
    val decoded = decode(frenchCode,secret)
    println("secret is "+decoded)
    assert(huffman === decoded)
  }  
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      println("encode ab as "+ encoded)
      println("decoded as "+decode(t1,encoded))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
