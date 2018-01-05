object MyObject extends App {

  def factorial(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int) : Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, f: Int, s: Int) : Int =
      if (n <= 0) f
      else go(n-1, s, f + s)

    go(n, 0, 1)
  }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n+1 >= as.length) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
  //Console.println(factorial(5))
  //Console.println(fib(0))
  //val list = Array(3,2,2,1,1)
  //Console.println(isSorted(list, (x: Int, y: Int) => x >= y))



}
