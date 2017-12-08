package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    var chars = "(if (zero? x) max (/ 1 x))".toList
    println(chars + " : " + balance(chars))
    chars = "I told him (that it’s not (yet) done). (But he wasn’t listening)".toList
    println(chars + " : " + balance(chars))
    chars = ":-)".toList
    println(chars + " : " + balance(chars))
    chars = "())(".toList
    println(chars + " : " + balance(chars))

    println("Count Change")
    println("4, [1, 2] : " + countChange(4, List(1, 2)))
    println("7, [1, 2, 3] : " + countChange(7, List(1, 2, 3)))
    println("8, [1, 2, 3] : " + countChange(8, List(1, 2, 3)))

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balanceNR(chars: List[Char]): Boolean = {
    var bc = 0
    for (c <- chars) {
      if (c == '(') bc += 1
      else if (c == ')') {
        if (bc > 0) bc -= 1
        else return false
      }
    }
    if (bc == 0) true else false
  }

  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], braceCnt: Int): Boolean = {
      if (chars.isEmpty && braceCnt == 0) true
      else if (chars.head == '(') balanceIter(chars.tail, braceCnt + 1)
      else if (chars.head == ')' && braceCnt > 0) balanceIter(chars.tail, braceCnt - 1)
      else if (chars.head == ')' && braceCnt <= 0) false
      else balanceIter(chars.tail, braceCnt)
    }

    balanceIter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange01(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted.reverse
    var cnt: Int = 0

    println("start countChange()", money, sortedCoins)

    if (coins.isEmpty || money == 0) {
      println("end 0")
      return 0
    }
    if (money == sortedCoins.head) {
      cnt += 1
    }

    if (money % sortedCoins.sum == 0) cnt += 1

    //if (money % sortedCoins.head == 0 && sortedCoins.length == 1) cnt += 1
    else if (money > sortedCoins.head) {
      for (i <- 1 to money / sortedCoins.head) {
        println("Choose : ", sortedCoins.head)
        cnt += countChange01(money - sortedCoins.head * i, sortedCoins)
      }
    }

    cnt += countChange01(money, sortedCoins.tail)

    println("end cnt", cnt)
    cnt
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted
    var cnt: Int = 0

    if (coins.isEmpty || money <= 0) return 0

    if (money == sortedCoins.head) cnt += 1
    else
      cnt += countChange(money - sortedCoins.head, sortedCoins)

    cnt += countChange(money, sortedCoins.tail)

    cnt

  }
}
