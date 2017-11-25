package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
  val a = singleton(1)

  def nth[T](n: Int, list: List[T]): T = {
    def loop(i: Int, list: List[T]): T =
      if (i < 0 || list.isEmpty) throw new IndexOutOfBoundsException
      else if (n == i) list.head
      else loop(i + 1, list.tail)

    loop(0, list)
  }

  val b = new Cons[Int](1, new Cons[Int](3, new Cons[Int](5, new Cons[Int](7, new Cons[Int](8, new Nil[Int])))))
  println(nth(3, b))
}


