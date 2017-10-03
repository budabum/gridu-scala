package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    def disc = b() * b() - 4 * a() * c()
    Signal(disc)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      Set(
        (-b() - Math.sqrt(delta())) / 2 * a(),
        (-b() + Math.sqrt(delta())) / 2 * a()
      )
    )
  }
}
