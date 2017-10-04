package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    def b1 = b()
    def delta = b1 * b1 - 4 * a() * c()
    Signal(delta)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def b1 = b()
    def delta1 = delta()
    if(delta1 < 0) Signal(Set(0))
    else Signal(
      Set(
        (-b1 - Math.sqrt(delta1)) / 2 * a(),
        (-b1 + Math.sqrt(delta1)) / 2 * a()
      )
    )
  }
}
