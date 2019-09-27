package com.peknight.fp.fpinscala.parallelism

trait Par[+A] {
}
object Par {
  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
  def fork[A](a: => Par[A]): Par[A] = ???
}
