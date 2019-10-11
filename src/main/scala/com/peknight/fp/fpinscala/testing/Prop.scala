package com.peknight.fp.fpinscala.testing

import Prop._
import com.peknight.fp.state.RNG

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Result = ???

  def &&(p: Prop): Prop = Prop{ (max, n, rng) =>
    val result = Prop.this.run(max, n, rng)
    result match {
      case Passed => p.run(max, n, rng)
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    val result = Prop.this.run(max, n, rng)
    result match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case s => s
    }
  }

  def tag(msg: String) = Prop { (max, n, rng) =>
    Prop.this.run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(a)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def follAll[A](g: SGen[A])(f: A => Boolean): Prop = ???

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
