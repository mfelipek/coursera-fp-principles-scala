import scala.math.abs

object session {

  def sqrtIter(guess: Double, x: Double): Double = {

    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs(guess * guess - x) / x < 0.000001
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double): Double = sqrtIter(1, x)
}

session.sqrt(2)

session.sqrt(4)

session.sqrt(0.001)
session.sqrt(0.1e-20)