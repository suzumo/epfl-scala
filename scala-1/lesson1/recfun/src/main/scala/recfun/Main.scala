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
    def pascal(c: Int, r: Int): Int = c match {
      case 0 => 1
      case `r` => 1
      case _ => if (c < 0) 0
                else if (c > r) -1
                else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def matchOpenParen(chars: List[Char]): Boolean = {
        if (chars.isEmpty) true
        else chars.head match {
          case ')' => false
          case '(' => matchCloseParen(chars.tail)
          case _   => matchOpenParen(chars.tail)
        }
      }

      def matchCloseParen(chars: List[Char]): Boolean = {
        if (chars.isEmpty) false
        else chars.last match {
          case '(' => false
          case ')' => matchOpenParen(chars.dropRight(1))
          case _   => matchCloseParen(chars.dropRight(1))
        }
      }

      matchOpenParen(chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var num = 0
      if (money <= 0) num = 0
      else {
        for (i <- 0 to coins.length-2) {
          num = num + countChangeR(money, coins, i)
        }
      }

      def countChangeR(money: Int, coins: List[Int], i: Int): Int = money match {
        case 0 => 1
        case _ => if (money < 0) 0
                  else if (i >= coins.length) 0
                  else countChangeR(money - coins(i), coins, i) + countChangeR(money, coins, i+1)
      }
      num
    }
  }