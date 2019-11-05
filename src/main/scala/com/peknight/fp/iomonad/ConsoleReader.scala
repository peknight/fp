package com.peknight.fp.iomonad

import com.peknight.fp.monad.Monad

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(r => f(run(r)))
  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(r => f(run(r)).run(r))
}
object ConsoleReader {
  import scala.language.implicitConversions
  implicit val monad = new Monad[ConsoleReader] {
    def unit[A](a: => A) = ConsoleReader(_ => a)
    def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
  }
}
