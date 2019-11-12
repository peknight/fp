package com.peknight.fp.streamingio

import com.peknight.fp.streamingio.Process._

sealed trait Process[-I, +O] {
  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await((i: I) => f(i, z) match {
    case (o, s2) => emit(o, loop(s2)(f))
  })

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }


  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = Await {
      case Some(d) => Emit(d + acc, go(d + acc))
      case None => Halt()
    }
    go(0.0)
  }
  def sumViaLoop: Process[Double, Double] = loop(0.0)((d: Double, acc: Double) => (d + acc, d + acc))

  def take[I](n: Int): Process[I, I] =
    if (n <= 0) Halt()
    else await { i => emit(i, take[I](n - 1)) }

  def drop[I](n: Int): Process[I, I] =
    if (n <= 0) id
    else await(_ => drop[I](n - 1))

  def takeWhile[I](f: I => Boolean): Process[I, I] = await { i =>
    if (f(i)) emit(i, takeWhile(f))
    else Halt()
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = await { i =>
    if (f(i)) dropWhile(f)
    else emit(i, id)
  }

  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = await(_ => emit(n + 1, go(n + 1)))
    go(0)
  }
  def countViaLoop[I]: Process[I, Int] = loop(0)((_: I, n: Int) => (n + 1, n + 1))

  def mean: Process[Double, Double] = {
    def go(total: Double, count: Int): Process[Double, Double] =
      await(d => emit((d + total) / (count + 1), go(total + d, count + 1)))
    go(0.0, 0)
  }
}

object Process {
  case class Emit[-I, +O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
  case class Await[-I, +O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Halt[-I, +O]() extends Process[I, O]

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] = Emit(head, tail)

  def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  /* The identity `Process`, just repeatedly echos its input. */
  def id[I]: Process[I,I] = lift(identity)
}
