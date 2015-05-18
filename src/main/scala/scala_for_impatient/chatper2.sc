/*
Q1
The signum of a number is 1 if the number is positive, ?1 if it is negative, and
0 if it is zero. Write a function that computes this value.
*/
def signum(x:Int) = {
  if(x>0) 1 else {if(x==0) 0 else -1}
}

signum(0)
signum(1)
signum(-100)

//Q2 - value and type of an empty block expression
//A: () , Unit
val ee = {}

//Q3
//Come up with one situation where the assignment x = y = 1 is valid in Scala
//var y:Int
//var x:Unit
//x = y = 1

/*
Q4
scala equivalent of Java code:
for (int i = 10; i >= 0; i--) System.out.println(i);
*/
for (i <- 10 to 0 by -1 ) yield i

/*
Q5
Write a procedure countdown(n: Int) that prints the numbers from n to 0.
*/
def countdown(n:Int) = {
  for(i <- n to 0 by -1) yield(i)
}
countdown(10)

/*
Q6
Write a for loop for computing the product of the Unicode codes of all letters
in a string.
*/
var x = 1
for (c <- "Hello") x *= c.toInt
x
//Q7 - Solve the preceding exercise without writing a loop
"Hello".foldLeft(1)((b,a) =>{ b*a.toInt})

/*
Q8
Write a function product(s : String) that computes the product,
as described in the preceding exercises.
*/
def product(s:String):Int = {
  s.foldLeft(1)((b,a) =>{ b*a.toInt})
}
product("Hello")

/*
Q9
Make the function of the preceding exercise a recursive function
*/
def product2(s:String):Int = {
  def loop(p:Int,s:String):Int = {
    if(s.length == 1)
      p*s(0).toInt
    else
      p*s(0).toInt*loop(p*s(0).toInt,s.tail)
  }
  loop(1,s)
}
product2("Hello")

//Q10 - Write a function that computes x^n
def xn(x:Int,n:Int):Double = {
  if(n>0){
    if(n%2 ==0) xn(x,n/2)*xn(x,n/2)
    else{
      x*xn(x,n-1)
    }
  }
  else{
    if(n==0) 1
    else{
      1/xn(x,-n)
    }
  }
}

xn(2,2)
xn(2,10)
xn(2,-10)
xn(2,0)
xn(4,4)


