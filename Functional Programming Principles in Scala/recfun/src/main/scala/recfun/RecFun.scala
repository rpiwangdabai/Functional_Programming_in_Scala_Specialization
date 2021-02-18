package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (!(c == 0) && !(r == 0) && r != c) {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    } else {
      1
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(chars: List[Char], leftNum:Int): Boolean = {
      if (leftNum < 0){
        false
      } else if(chars.isEmpty) {
        leftNum == 0
      } else{
        val c = chars.head
        val tailChars = chars.tail
        val newNum = if (c == '('){
          leftNum + 1
        } else if (c == ')') {
          leftNum - 1
        } else leftNum
        helper(tailChars, newNum)
      }
    }
    helper(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0){
      1
    } else if (money < 0){
      0
    } else {
      if (coins.isEmpty){
        0
      } else {
        val coinsTail = coins.tail
        val h = coins.head
        countChange(money - h, coins) + countChange(money, coinsTail)
      }
    }
  }
}
