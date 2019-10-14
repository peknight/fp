package com.peknight.fp.testing

import java.util.concurrent.Executors

import Prop._
import com.peknight.fp.testing.Gen.{choose, unit, weighted}
import com.peknight.fp.parallelism.Nonblocking.Par
import com.peknight.fp.state.RNG

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
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
  type TestCases = Int // 生成多少个测试用例
  type MaxSize = Int // 每个测试用例生成器的最大size

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(a)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    // For each size, generate this many random cases.
    val casesPerSize = (n + (max - 1)) / max
    // Make one property per size, but no more than n properties
    val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _) // Combine them all into one property
    prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 10, testCases: Int = 20, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run (maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case s ** a => Par.run(s)(f(a)) }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}
