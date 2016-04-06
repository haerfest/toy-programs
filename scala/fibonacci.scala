object Application {

  def fib(n: Int): Int = {
    def inner(n: Int, a: Int, b: Int): Int = {
      if (n == 0)
        a
      else
        inner(n - 1, b, a + b)
    }
    inner(n, 0, 1)
  }

  def timeIt(f: () => Unit): Unit = {
    val startTime = System.nanoTime()
    f()
    val endTime = System.nanoTime()
    val elapsed = (endTime - startTime) / 1e6
    println(s"elapsed: $elapsed msec");
  }

  def main(args: Array[String]): Unit = {
    timeIt(() => println(fib(41)))
  }
}
