//def sum(f: Int => Int)(a: Int, b: Int): Int = {
//  def loop(a: Int, acc: Int): Int = {
//    if (a > b) acc
//    else loop(a + 1, acc + f(a))
//  }
//  loop(a, 0)
//}



//def product(f: Int => Int)(a: Int, b: Int): Int = {
//  def loop(a: Int, acc: Int): Int =
//    if (a > b) acc
//    else loop(a + 1, acc * f(a))
//
//  loop(a, 1)
//}


def general(op: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, op(acc, f(a)))
  loop(a, unit)
}

val sum = general((a, b) => a + b, 0)_
val product = general((a, b) => a * b, 1)(_)

sum(x => x * x)(1, 100)


def fact(n: Int) = product(x => x)(1, n)

fact(1)
fact(2)
fact(3)
fact(4)
fact(5)