package recfun
import scala.annotation.tailrec


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
    def pascal(c: Int, r: Int): Int =
      if (r == 0 && c ==0) 0
      else if (r == 0 || c == 0) 1
      else pascal(c-1,r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

    @tailrec def helperBalance(sum:Int, chars: List[Char]): Boolean = {
        if  (sum < 0) false
        else if (sum == 0 && chars.isEmpty) true
        else if (chars.head == '(') helperBalance(sum+1, chars.tail)
        else if (chars.head == ')') helperBalance(sum-1, chars.tail)
        else helperBalance(sum, chars.tail)
      }

      helperBalance(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (coins.isEmpty) 0
      else if (money == 0)  1
      else countChange( money, coins.tail) + countChange(money-coins.head, coins)
    }
  }
