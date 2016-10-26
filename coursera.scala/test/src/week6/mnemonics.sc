package week6

import scala.io.Source

object mnemonics {
  val in = Source.fromFile("D:\\DATA\\Projects\\scala\\forcomp\\src\\main\\resources\\forcomp\\linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
  val mnem = Map (
  	'2' -> "ABC",'3'->"DEF" ,'4'->"GHI",'5'->"JKL",
  	'6' -> "MNO",'7'->"PQRS",'8'->"TUV",'9'->"WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,java.lang.String] = Map(8 -> TUV
                                                  //| , 4 -> GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
  
  //Invert the mnem map to give a map from Chars 'A' ... 'Z' to '2' ... '9'
  val charCode:Map[Char,Char] = for { (k,v) <- mnem; i<-v } yield i->k
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)
 	//Maps a word to the digit string it can represent, e.g. "Java"->"5282"
 	def wordCode(word:String):String = //for {s <- word.toUpperCase} yield charCode(s) /*another solution*/
      word.toUpperCase map charCode //use Map as a function
                                                  //> wordCode: (word: String)String
 	
 	/* A map from digit strings to the words that reresent them
 	   e.g. "5282" -> List("Java","Kata","Lava",...)
 	   Note: A missing number should map to the empty set, e.g. "1111" -> List()
 	 */
 	def wordsForNum: Map[String,Seq[String]] = words groupBy wordCode withDefaultValue Seq()
                                                  //> wordsForNum: => Map[String,Seq[String]]

  //Return all ways to encode a number as a list of words
	def encode(number:String):Set[List[String]] = {
		if(number.isEmpty) Set(List())
		else{
			for{
				split <- 1 to number.length
				word  <- wordsForNum(number take split)
				rest  <- encode(number drop split)
			}	yield word::rest
		}.toSet
	}                                         //> encode: (number: String)Set[List[String]]
 	
 	
 	wordCode("Java")                          //> res0: String = 5282
 	wordsForNum                               //> res1: Map[String,Seq[String]] = Map(63972278 -> List(newscast), 29237638427
                                                  //|  -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List(allyin
                                                  //| g), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 868437 -> L
                                                  //| ist(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3364646489
                                                  //|  -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 -> List
                                                  //| (pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386583 -> 
                                                  //| List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827 ->
                                                  //|  List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 84
                                                  //| 863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464 -> Lis
                                                  //| t(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers)
                                                  //| , 46636233 -> List(homemade), 7446768759 -> List(rigorously), 74644647 -> L
                                                  //| ist(ringings), 633738 -> List(offset), 847825 -> List(visual), 772832 -> Li
                                                  //| st(Pravda), 4729378 -> 
                                                  //| Output exceeds cutoff limit.
  
  val encoded = "7225247386"                      //> encoded  : java.lang.String = 7225247386
	encode(encoded)                           //> res2: Set[List[String]] = Set(List(sack, ah, re, to), List(rack, bird, to),
                                                  //|  List(sack, air, fun), List(rack, ah, re, to), List(pack, bird, to), List(s
                                                  //| ack, bird, to), List(pack, air, fun), List(Scala, ire, to), List(Scala, is,
                                                  //|  fun), List(pack, ah, re, to), List(rack, air, fun))
                                                  
	def translate(number:String):Set[String] =
		encode(number) map ( _ mkString " ")
                                                  //> translate: (number: String)Set[String]
		
	translate(encoded)                        //> res3: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)
	                                                
}