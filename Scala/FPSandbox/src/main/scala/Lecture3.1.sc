object intsets {
  println("Hello world!")
  val t1 = new NonEmpty(13, new Empty, new Empty)
  val t2 = t1 incl 14
  val t3 = t2 incl 12

  val t4 = new NonEmpty(7, new Empty, new Empty) incl 5 incl 8
  val t5 = t3 union t4
  val t6 = t5 incl 100 incl 17 incl 1
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

//class Empty extends IntSet {
//  def contains(x: Int): Boolean = false
//
//  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//
//  override def toString = "."
//}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right:IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem)new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem


  override def toString: String = "{" + left + elem + right + "}"
}


//object overrides {
//  val c1: Base = new Sub
//  println(c1.foo, c1.bar)
//}
//
//abstract class Base {
//  def foo = 1
//  def bar: Int
//}
//
//class Sub extends Base {
//  override def foo = 2
//  def bar = 3
//}