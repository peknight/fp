package com.peknight.fp.fpinscala.monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
object Monoid {
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }
}
