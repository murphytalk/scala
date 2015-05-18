10 max 2
"Crazy" * 3
"Hello"(4)
BigInt(2) pow 1024

def v(x:Int) =
  if (x > 0) 1 else ()
v(0) == ()
v(1) == ()

for (i <- 1 to 3; from = 4 - i; j <- from to 3) yield((10 * i + j) + " ")