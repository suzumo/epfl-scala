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
      var num = 0
      var uniqueCoins = List[Int]()

      // this doesn't work cos can't figure out how to append to lists
      coins.foreach { coin => if (!uniqueCoins.contains(coin)) uniqueCoins.append(coin) }

      if (money <= 0) num = 0
      else {
        for (i <- 0 to uniqueCoins.length-2) {
          num = num + countChangeR(money, uniqueCoins, i)
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