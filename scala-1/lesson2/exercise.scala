import math.abs

class Rational(x: Int, y: Int) {
	require(y > 0, "denominator must be non-zero")
	// can also
	// assert(y > 0)
	def numer = x / g
	def denom = y / g

	private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
	private val g = gcd(x, y)


	def < (that: Rational) = numer * that.denom < that.numer * denom

	def max(that: Rational) = if (this < that) that else this

	def unary_- : Rational = new Rational (-numer, denom)

	def + (that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
	def - (that: Rational) = this + -that

	override 
	def toString = numer + "/" + denom
}

object exercise {
	def product (f: Int => Int)(a: Int, b: Int): Int =
		if (a > b) 1
		else f(a) * product (f)(a + 1, b)
	
	product(x => x * x) (3, 4)

	def factorial(n: Int) = product(x => x) (1, n)

	val tolerance = 0.0001
	def isCloseEnough(x: Double, y: Double) = 
		abs((x - y) / x) < tolerance

	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
		def iterate(guess: Double): Double = {
			val next = f(guess)
			if (isCloseEnough(guess, next)) next
			else iterate(next)
		}
		iterate (firstGuess)
	}

	val x = new Rational(1,2)
}