/*
import math.Ordering
object mergesort{
	def msort[T](xs:List[T])(implicit ord:Ordering[T]):List[T] = {
		val n =xs.length / 2
		if (n==0) xs
		else{
			def merge(xs:List[T],ys:List[T]):List[T] = ( xs,ys) match {
				case (Nil,ys) => ys
				case (xs,Nil) => xs
				case (x ::xs1, y::ys1) => if(ord.lt(x, y)) x :: merge(xs1,ys) else y::merge(xs,ys1)
			}
			
			val (fst,snd) = xs splitAt n
			merge(msort(fst),msort(snd))
		}
	}
	
	msort(List(100,-1,0,20))
}
*/
object week5 {
	def pack1[T](xs:List[T]) : List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 => {
			val l = pack1(xs1)
			if( l == Nil) List(List(x))
			else{
				val l1 = l.head
				if(l1.head == x) (x::l1)::l.tail
				else List(x) :: l
			}
		}
	}                                         //> pack1: [T](xs: List[T])List[List[T]]

	def pack2[T](xs:List[T]) : List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 => {
			pack2(xs1) span ( o => o.head == x) match {
				case (Nil,Nil) => List(List(x))
				case (Nil,l)   => List(x) :: l
				case (l,Nil)   => List(x) :: l
				case (l1,l2)   => (x::l1.head) :: l2
			}
		}
  }                                               //> pack2: [T](xs: List[T])List[List[T]]

	def pack[T](xs:List[T]) : List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 =>
		   val (first, rest) = xs span (y => y==x)
		   first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

	
	val l = List("a","a","a","b","c","c","a") //> l  : List[java.lang.String] = List(a, a, a, b, c, c, a)
	
	pack1(l)                                  //> res0: List[List[java.lang.String]] = List(List(a, a, a), List(b), List(c, c
                                                  //| ), List(a))
                                                  
  pack2(l)                                        //> res1: List[List[java.lang.String]] = List(List(a, a, a), List(b), List(c, c
                                                  //| ), List(a))
                                                  
  pack(l)                                         //> res2: List[List[java.lang.String]] = List(List(a, a, a), List(b), List(c, c
                                                  //| ), List(a))
                                                  
                                                  
	def encode[T](xs:List[T]) : List[(T,Int)] = {
		pack(xs) map ( x => (x.head,x.length))
	}                                         //> encode: [T](xs: List[T])List[(T, Int)]
	
	encode(l)                                 //> res3: List[(java.lang.String, Int)] = List((a,3), (b,1), (c,2), (a,1))
	
	
	l.reduceLeft( (x,y) => x+y)               //> res4: java.lang.String = aaabcca
	
}