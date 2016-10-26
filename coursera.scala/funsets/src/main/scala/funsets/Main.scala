package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  /*
  printSet(union(x =>x>=0 && x<=100 , x =>x >= -100 && x<=0))
  printSet(intersect(x =>x>=0 && x<=100 , x =>x >= -100 && x<=0))
  printSet(diff(x =>x>=0 && x<=100 , x =>x >= -100 && x<=0))
  printSet(map(x=> x>=1 && x<=4,x=>x*2))
  */
}
