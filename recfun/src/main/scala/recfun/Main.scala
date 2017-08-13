package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    balance("()(".toList)
  }

  def factorial(n: Int): Int = {
    if (n < 2) 1
    else n * factorial(n-1)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      val z = (factorial(c) * factorial(r-c))
      if (z == 0) 1
      else factorial(r)/ z
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def additional_check(res: Boolean, ints: List[Int]): Boolean ={
        if (ints.isEmpty) true
        else {
          val sum = ints.sum
          val curRes = sum >= 0
          res & additional_check(curRes, ints.init)
        }
      }
      def f(c: Char) = if (c == '(') 1 else if (c == ')') -1 else 0
      val ints = chars.map(e => f(e)).filterNot(_ == '0')
      if (ints.length %2 != 0 || ints.sum != 0) false
      else additional_check(res = true, ints)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def cc(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else {
          if (money <0 || coins.isEmpty) 0
          else cc(money, coins.init) + cc(money - coins.last, coins)
        }
      }

      cc(money, coins.sorted)
    }
  }
