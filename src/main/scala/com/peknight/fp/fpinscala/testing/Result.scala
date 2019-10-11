package com.peknight.fp.fpinscala.testing

import Result._

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Result {
  type FailedCase = String
  type SuccessCount = Int
}