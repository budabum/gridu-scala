val li1a = List(1,2,3,4,5)
val li1b = List(1,2,3,4)
val li2a = List(33,31,30,27,7)
val li2b = List(33,31,27,7)
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
