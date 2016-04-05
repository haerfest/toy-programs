def fib(n: Int): Int = {
  def inner(n: Int, a: Int, b: Int): Int = {
    if (n == 0)
      a
    else
      inner(n - 1, b, a + b)
  }
  inner(n, 0, 1)
}

val n = 41
println(s"fib($n) = ${fib(n)}")
