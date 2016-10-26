object assignment6 {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  def combinations(occurrences: Occurrences): List[Occurrences] = {
	  if (occurrences.isEmpty) List()
	  else{
		  val tail = combinations(occurrences.tail)
		  val l = (for ( i<- 1 to occurrences.head._2) yield (occurrences.head._1,i)).toList
		  tail ::: (for (i <- l;j <- tail) yield i::j).toList
	  }
  }                                               //> combinations: (occurrences: assignment6.Occurrences)List[assignment6.Occurre
                                                  //| nces]


  
  val l = List(('a', 2), ('b', 2))                //> l  : List[(Char, Int)] = List((a,2), (b,2))
  
  combinations(l)                                 //> res0: List[assignment6.Occurrences] = List()
  
  
  
  val n = 7                                       //> n  : Int = 7
  
	for{
			i<- 1 until n
	    j<- 1 until i
	}yield (i,j)                              //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5))
	      
}