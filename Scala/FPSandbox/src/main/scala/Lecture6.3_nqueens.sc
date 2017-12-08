object nqueens{
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }

    placeQueens(n)
  }

  /**
    * which test is a queen placed in an indicated column col is secure
    * amongst the other place queens.
    * It is assumed that the new queens is placed in the next available row
    * after the other placed queens (in other words: in row queens.length).
    *    - 상하/좌우/대각선에 Queen 없음 확인
    * @param col
    * @param queens
    * @return
    */
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row-1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  val result = queens(8)
  result.size

  (result take 3 map show) mkString "\n"



}