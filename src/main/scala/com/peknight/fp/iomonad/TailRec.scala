package com.peknight.fp.iomonad

import com.peknight.fp.monad.Monad

import scala.annotation.tailrec

sealed trait TailRec[A] {
  import TailRec._
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
}
object TailRec extends Monad[TailRec] {
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
  @tailrec
  def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => {
      x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        /*
         * FlatMap(FlatMap(y, g), f)
         * y.flatMap(g).flatMap(f)
         * y.flatMap(a => g(a).flatMap(f)) // Monad Associativity Law
         * y.flatMap(a => FlatMap(g(a), f))
         * FlatMap(y, a => FlatMap(g(a), f))
         */
        case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
      }
    }
  }

  def unit[A](a: => A): TailRec[A] = Suspend(() => a)
  def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]) = fa flatMap f
  def apply[A](a: => A): TailRec[A] = unit(a)
}
