package com.peknight.fp.applicative

sealed trait Validation[+E, +A]
object Validation {
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]
}
