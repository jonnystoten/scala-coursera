package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) 1
      else if (c == r) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(acc: Int, chars: List[Char]): Boolean =
        if (acc < 0) false
        else if (chars.isEmpty) acc == 0
        else {
          val newAcc = if (chars.head == '(') acc + 1
          else if (chars.head == ')') acc - 1
          else acc
          balanceIter(newAcc, chars.tail)
        }

      balanceIter(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeSorted(money: Int, coins: List[Int]): Int =
        if (money == 0) 1
        else if (coins.isEmpty || money < 0) 0
        else countChangeSorted(money - coins.head, coins) + countChangeSorted(money, coins.tail)

      countChangeSorted(money, coins.sorted.reverse)
    }
  }
