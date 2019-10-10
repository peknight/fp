package com.peknight.fp.fpinscala.testing

import com.peknight.fp.state.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def choose2(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n, rng2) => (start + n % (stopExclusive - start), rng2)
  }))

}
