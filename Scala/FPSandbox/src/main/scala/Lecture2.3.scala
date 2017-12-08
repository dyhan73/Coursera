object main extends App {
  val tolerance = 0.0001

  def isCloseEnough(x : Double, y: Double): Boolean =
    if ((math.abs((x - y) / x) / x) < tolerance) true else false

  def fixedPoint(f: Double => Double)(fistGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(fistGuess)
  }

  println(fixedPoint(x => 1 + x/2)(1))

  println("Hello world")
}