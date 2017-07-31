package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
  {
    val B= b()

    Signal(B*B - 4*a()*c())

  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val delta = computeDelta(a,b,c).apply()

    Signal {
      if (delta > 0) Set((-b() + math.sqrt(delta)) / 2 * a(), (-b() - math.sqrt(delta)) / 2 * a())

      else Set()

    }
  }
}
