val li1a = List(1,2,3,4,5)
val li1b = List(1,2,3,4)
val li2a = List(33,31,30,27,7)
val li2b = List(16,25, 16, 9, 81,144,-4)
val li3a = List(1,133,33,231,27,7)
val li3b = List(1,133,33,231,27,7,16)


def merge(xs: List[Int], ys: List[Int]): List[Int] = {
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case  (x :: xs1, y :: ys1) =>
      if(x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }
}

def merge2(xs: List[Int], ys: List[Int]): List[Int] = {
  xs match {
    case Nil => ys
    case x :: xs1 =>
      ys match {
        case Nil => xs
        case y :: ys1 =>
          if(x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
  }
}

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if(n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

msort(li1a)
msort(li1b)
msort(li2a)
msort(li2b)
msort(li3a)
msort(li3b)

def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => Math.pow(y, 0.5).toInt :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (e => Math.pow(e, 0.5).toInt)

squareList1(li2b)
squareList1(li2b)

def pack1[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    xs.takeWhile(e => e == x) :: pack1(xs.dropWhile(e => e == x))
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (fst, snd) = xs.span(e => e==x)
    fst :: pack(snd)
}

val li5a = List("a", "a", "a", "b", "c", "c", "a")
pack(li5a)
pack1(li5a)

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(e => (e.head, e.size))
}

encode(li5a)

li1a.foldRight(List[Int]())((e,a) => e*7 :: a)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, ys) => f(x) :: ys)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, sum) => sum + 1 )

li3a.size
lengthFun(li3a)

li1a.map(e=>e+1)
li5a.map(e=>e.toUpperCase)
mapFun(li1a, (e:Int)=>e+1)
mapFun(li5a, (e:String)=>e.toUpperCase)

