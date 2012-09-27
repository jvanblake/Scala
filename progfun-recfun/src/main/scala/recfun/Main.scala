package recfun
import common._

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
    if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.head == '(') balance2(chars.tail, 1)
    else if (chars.head != ')') balance2(chars.tail, 0)
    else false
  }

  def balance2(chars: List[Char], total: Int): Boolean = {
    if (total < 0 || (chars.isEmpty && total != 0)) false
    else if (total == 0 && chars.isEmpty) true
    else if (chars.head == '(') balance2(chars.tail, total + 1)
    else if (chars.head == ')') balance2(chars.tail, total - 1)
    else balance2(chars.tail, total)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else (
      if (money >= coins.head) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else (countChange(money, coins.tail))
      )
  }
}
