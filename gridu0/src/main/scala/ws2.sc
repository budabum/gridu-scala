def sum2(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum2(f, a+1, b)

def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}

def sum3_0(f: Int => Int): (Int, Int) => Int = {
  def loop(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + loop(a+1, b)
  }
  loop
}

def sum3(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum3(f)(a+1, b)

def sumInts(a: Int, b: Int) = sum(num, a, b)
def sumCubes(a: Int, b: Int) = sum(cube, a, b)
def sumFacts(a: Int, b: Int) = sum(fact, a, b)

def sumInts3 = sum3_0(x => x)
def sumCubes3 = sum3_0(x => x * x * x)
def sumFacts3 = sum3_0(fact)

def num(x: Int) = x
def cube(x: Int) = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

sum2(num, 1, 4)
sum2(num, 1, 9)
sum2(cube, 1, 3)
sum2(fact, 1, 3)
sum2(fact, 1, 4)

sum3(num)(1, 4)
sum3(num)(1, 9)
sum3(cube)(1, 3)
sum3(fact)(1, 3)
sum3(fact)(1, 4)

sumInts(1,4)
sumInts(1,9)
sumCubes(1,3)
sumFacts(1,3)
sumFacts(1,4)

sumInts3(1,4)
sumInts3(1,9)
sumCubes3(1,3)
sumFacts3(1,3)
sumFacts3(1,4)

def prod(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * prod(f)(a+1, b)

def fact2(n: Int) = prod(x => x)(1, n)

def proto(u: Int, m:(Int,Int) => Int, f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) u
  else m(f(a), proto(u, m, f)(a+1, b))

prod(x => x)(1,4)
prod(x => x * x)(1,3)
prod(x => x+1)(1,4)

fact(1)
fact(3)
fact(5)

fact2(1)
fact2(3)
fact2(5)

proto(0, (x,y) => x+y, x=>x)(1,9)
proto(1, (x,y) => x*y, x=>x)(1,4)
