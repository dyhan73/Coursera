
object lecture6 {

  // Combinatorial search
  val n = 7

  (1 until n)

  val pairs = (1 until n) map (i =>
    (1 until i) map (j => (i, j)))

  val list = pairs.flatten.toList

  val fmap = (1 until n) flatMap (i => (1 until i) map (j => (i, j)))

  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

  fmap filter (pair => isPrime(pair._1 + pair._2))


  // Lecture 6.2 for-expression

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for ((x, y) <- xs zip ys) yield x*y).sum
  }

  scalarProduct(List(1, 2), List(3,4))


  // Lecture 6.3 Sets
  val fruit = Set("apple", "banana", "pear")
  val s = (1 to 6).toSet

  s map (_ + 2)
  fruit filter (_.startsWith("app"))
  s.nonEmpty


  // Lecture 6.4 Map

  // define
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern", ("Korea", "Seoul"))

  // Iterable
  val countryOfCapital = capitalOfCountry map {
    case (x, y) => (y, x)
  }

  // Maps are functions
  capitalOfCountry("Korea")

  // using default value
  val cap1 = capitalOfCountry withDefaultValue "<unkonwn>"
  cap1("Andorra")

  //capitalOfCountry("Canada")  // raising exception

  // get value without key existence
  capitalOfCountry get "US"     // Some
  capitalOfCountry get "Japan"  // None

  // Decomposing Option
  def showCapital(country: String) = capitalOfCountry get country match {
    case Some(capital) => capital
    case None => "Missing Data"
  }

  showCapital("US")
  showCapital("Japan")

  // Sorted and GroupBy
  val fruit2 = List("apple", "pear", "orange", "pineapple")
  fruit2 sortWith(_.length < _.length)
  fruit2 sorted

  fruit2 groupBy (_.head)

  // Polynomials
  class Poly(val terms: Map[Int, Double]) {
    def + (other: Poly) =
      // new Poly(terms ++ other)  // 같은 exp 가 Key 로 존재하는 경우 other 이 terms를 덮어버림
      new Poly(terms ++ (other.terms map adjust))  // 같은 exp Key 존재 시 coeff 합쳐줌
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }
    }

    override def toString: String =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  p1 + p2

  // usign withDefaultValue -> adjust 함수를 단순화 하는데 유용함
  // 바인딩 파라미터  변환 생성자 추가
  class Poly2(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly2) =
    // new Poly(terms ++ other)  // 같은 exp 가 Key 로 존재하는 경우 other 이 terms를 덮어버림
      new Poly2(terms ++ (other.terms map adjust))  // 같은 exp Key 존재 시 coeff 합쳐줌
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString: String =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p3 = new Poly2(1->2.0, 3->4.0, 5->6.2)
  val p4 = new Poly2(Map(0->3.0, 3->7.0))
  p3 + p4

  // map 과 ++ 를 사용하지 않는 + 연산자 정의
  class Poly3(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly3) =
      new Poly3(other.terms foldLeft terms)(addTerm)
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString: String =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p5 = new Poly3(1->2.0, 3->4.0, 5->6.2)
  val p6 = new Poly3(Map(0->3.0, 3->7.0))
  p5 + p6

}
