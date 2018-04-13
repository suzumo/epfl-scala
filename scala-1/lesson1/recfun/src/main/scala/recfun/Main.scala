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
      var openParen = 0

      def parseStr(chars: List[Char]): Boolean = {
        if (chars.isEmpty) openParen == 0
        else {
          chars.head match {
            case '(' => matchOpenParen(chars.tail)
            case ')' => matchCloseParen(chars.tail)
            case _   => parseStr(chars.tail)
          }
        }
      }

      def matchOpenParen(chars: List[Char]): Boolean = {
        openParen = openParen + 1
        parseStr(chars)
      }

      def matchCloseParen(chars: List[Char]): Boolean = {
        openParen = openParen - 1
        if (openParen < 0) false
        else parseStr(chars)
      }

      parseStr(chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var num: Int = 0
      var uniqueCoins = List[Int]()

      def countChangeR(money: Int, coins: List[Int], i: Int): Int = {

        if (money < 0) 0
        else if (i == coins.length) 0 // there's no more coins to compare
        else if (money == 0) 1 // perfect change
        else countChangeR(money - coins(i), coins, i) + countChangeR(money, coins, i+1)       // take/don't take coin(i)
      }

      coins.foreach { coin => if (!uniqueCoins.contains(coin)) uniqueCoins = coin :: uniqueCoins }

      if (money <= 0) 1
      else if (coins.isEmpty) 0
      else countChangeR(money, uniqueCoins, 0)
    }
  }