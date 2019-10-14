package com.peknight.fp.testing

import com.peknight.fp.state.{RNG, State}

case class Gen[+A](sample: State[RNG, A]) {
  import Gen._

  def map[B](f: A => B): Gen[B] = flatMap(a => unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap { n => this.listOfN(n) }

  def unsized: SGen[A] = SGen(_ => Gen.this)

  def map2[B,C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))
}

object Gen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n max 1, g))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def choose2(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n, rng2) => (start + n % (stopExclusive - start), rng2)
  }))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = double.flatMap { d =>
    if (d < g1._2.abs / (g1._2.abs + g2._2.abs)) g1._1 else g2._1
  }

  trait Cogen[-A] {
    def sample(a: A, rng: RNG): RNG
  }

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = {
    Gen { State { (rng: RNG) =>
      val func: A => B = (a: A) => out.sample.run(in.sample(a, rng))._1
      (func, rng)
    }}
  }
}