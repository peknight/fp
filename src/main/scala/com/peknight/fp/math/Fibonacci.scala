package com.peknight.fp.math

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }


  def fib: LazyList[Int] = LazyList.unfold[Int, (Int, Int)]((0, 1)) {
    case (value0, value1) => Option((value0, (value1, value0 + value1)))
  }
}

