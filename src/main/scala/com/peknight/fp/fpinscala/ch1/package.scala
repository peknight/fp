package com.peknight.fp.fpinscala

package object ch1 {
  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, com.peknight.fp.math.Math.abs(x))
  }

  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, com.peknight.fp.math.Factorial.factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

}
