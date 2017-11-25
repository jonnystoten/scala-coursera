class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numerator = x / g
  def denominator = y / g

  def < (that: Rational) =
    numerator * that.denominator < that.numerator * denominator

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)

  def unary_- : Rational = new Rational(-numerator, denominator)

  def - (that: Rational) = this + -that

  override def toString = numerator + "/" + denominator
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
val big = new Rational(3e6.toInt, 2e6.toInt)

big - y - z
big + big
big < y
big max y

x + y
