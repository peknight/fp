package com.peknight.fp.fpinscala.testing

sealed trait Prop {
  import Prop._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  }
}
object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
