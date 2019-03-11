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


  def pascal(c: Int, r: Int): Int = {
    //  @tailrec
    def loop(acc: Int, col: Int, row: Int): Int = if (col == 0 || col == row) 1 else
      loop(acc, col - 1, row - 1) + loop(acc, col, row - 1)
    loop(0, c, r)
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def go(acc:Int , letters:List[Char]) : Int = {
        if(letters.isEmpty)
         acc
        else {
          if (letters.head == '(')
            go(acc + 1, letters.tail)
          else if (letters.head == ')' && acc > 0)
            go(acc - 1, letters.tail)
          else
            go(acc, letters.tail)
        }
      }

      def goCount(acc:Int , letters:List[Char]) : Int = {
        if(letters.isEmpty)
          return acc

        if(letters.head == '(')
          goCount(acc +1, letters.tail)
        else if(letters.head == ')')
          goCount(acc-1, letters.tail)
        else
          goCount(acc, letters.tail)
      }
      val x = go(0, chars)
      val y = goCount(0, chars)
      x == 0 && y == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def go(acc:Int, cash:Int, remCoins:List[Int]) :Int = {
          if(cash == 0)
            return acc + 1
          if(cash < 0)
            return acc
          if(remCoins.isEmpty)
            return acc
        go(acc, cash-remCoins.head, remCoins) + go(acc, cash, remCoins.tail)
//        go(acc, cash, remCoins.tail)
      }
      go(0, money, coins)
//      coins.map(item => go(0, money-item, coins)).sum
    }
  }
