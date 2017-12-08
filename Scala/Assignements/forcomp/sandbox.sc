//import forcomp.Anagrams.{Occurrences, Word, wordOccurrences}
//package forcomp

//import forcomp.Anagrams

object sandbox {


  val word = "workroo"

  word.toList

  val gbWord = word groupBy ((c: Char) => (c))
  println(gbWord.getClass)

  gbWord.toList.sorted

  //gbWord.map ((k, v) => (k, v.length))

  gbWord map { case (k, v) => (k, v.length) }


  //println(Anagrams.wordOccurrences("kangaroo"))

  val word2 = "wooords"
  word2 map ((c: Char) => c -> c) toMap //groupBy ((k: Char, v: Char) => (k))

//  val mWord2 = word2 map { case (c) => c -> c } toMap
//  println(mWord2)


  def wordOccurrences(w: String): List[(Char, Int)] = {
    w.toLowerCase.toList.groupBy((c:Char) => c).map {case (k, v) => (k, v.length)}.toList.sorted
  }

  val wordList = List("abc", "aabba", "eat", "ate", "tea")

  val t1 = wordList flatMap wordOccurrences
  t1.groupBy(x => x._1)
      // Map(b -> List((b,1), (b,2)), a -> List((a,1), (a,3)), c -> List((c,1)))
  val t2 = t1.groupBy(x => x._1) map {
    case (k, v) => v.foldLeft((k, 0))((m, n) => (m._1, m._2 + n._2))
  }
  t2.toList.sorted



  wordList map (w => (wordOccurrences(w), w)) groupBy(x => x._1) map {
    case (k, v) => v.foldLeft((k, ""))((m, n) => (m._1, m._2 + " " + n._2))
  } map {
    case (k, v) => (k, v.stripPrefix(" ").split(" ").toList.sorted)
  }

  // combination
  val combiList = List(('a', 2), ('b', 2))

  for (a <- combiList)
    for (i <- 0 to a._2) println((a._1, i))

  def combi(cl: List[(Char, Int)]): List[List[(Char, Int)]] = cl match {
    case List() => List(Nil)
    case (c, n) :: remain => {
      val tailList = combi(remain)
      tailList ::: (for {
        t <- tailList
        i <- 1 to n
      } yield (c, i) :: t)
    }
  }

  combi(combiList)


  // subtract
  def sub(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
    val yMap = ((x map {case(c, n) => (c, 0)}) ::: y).toMap.toSet.toMap
    val rslt = for {
      (c, n) <- x
    } yield (c, n - yMap(c))
    rslt.filter(_._2 > 0)
  }

  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
  val r = List(('r', 1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))

  //r('a')
  val temp = ((lard map {case (c, n) => (c, 0)}) ::: r).toMap.toSet.toMap


  sub(lard, r)

//  for {
//    (k1, v1) <- lard.toMap
//  }
}


