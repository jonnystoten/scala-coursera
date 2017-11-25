import week3.{Cons, Nil, List}

def nth[T](n: Int, list: List[T]): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else nth(n - 1, list.tail)

val b = new Cons[Int](1, new Cons[Int](3, new Cons[Int](5, new Cons[Int](7, new Cons[Int](8, new Nil[Int])))))
nth(3, b)
//nth(-1, b)
//nth(10, b)
