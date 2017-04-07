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
      // c!/r!(c-r)!
      if (c == 0) 1
      else factorial(r)/(factorial(c)*factorial(r-c))
    }

    def factorial(x: Int): Int = {
      def factItr(value: Int, fac: Int): Int = {
        if (value.equals(1)) fac
        else {
          factItr(value-1, value*fac)
        }
      }
      factItr(x, 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def iter(chars: List[Char], acc: Int) : AnyVal= {
        if(chars.isEmpty || acc < 0) acc
        else if (chars.head == '(') iter(chars.tail, acc+1)
        else if (chars.head == ')') iter(chars.tail, acc-1)
        else iter(chars.tail, acc)
      }
      iter(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else if (money <= 0 && !coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
