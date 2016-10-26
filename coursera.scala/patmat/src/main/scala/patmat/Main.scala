package patmat
import Huffman._
object Main{
	def main(args: Array[String]) {
	    val text = List('B','A','D')
	    val tree = createCodeTree(("A"*8).toList ::: ("B"*3).toList ::: 
	                               List('C') ::: List('D') ::: List('E') ::: 
	                               List('F') ::: List('G') ::: List('H'))    
	    val encoded = encode(tree)(text)   
	    val qencoded = quickEncode(tree)(text)
	    println("encoded as "+encoded)
	    println("qencoded as "+qencoded)
	    println("decoded "+decode(tree,encoded))
	    println("decoded from qencoded "+decode(tree,qencoded))
    }
}