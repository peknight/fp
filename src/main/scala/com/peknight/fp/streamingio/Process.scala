package com.peknight.fp.streamingio

import com.peknight.fp.iomonad.IO
import com.peknight.fp.monad.Monad
import com.peknight.fp.streamingio.Process._

import scala.annotation.tailrec

sealed trait Process[I, O] {
  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

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

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Emit(h, t) => t |> f(Some(h))
      case Halt() => Halt() |> f(None)
      case Await(g) => Await((i: Option[I]) => g(i) |> p2)
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] = Process.zip(this, p)

  def zipWithIndex: Process[I, (O, Int)] = this zip (count map (_ - 1))

  /* Add `p` to the fallback branch of this process */
  def orElse(p: Process[I,O]): Process[I,O] = this match {
    case Halt() => p
    case Await(recv) => Await {
      case None => p
      case x => recv(x)
    }
    case _ => this
  }
}

object Process {
  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Halt[I, O]() extends Process[I, O]

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

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await((i: I) => f(i, z) match {
    case (o, s2) => emit(o, loop(s2)(f))
  })

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  /* The identity `Process`, just repeatedly echos its input. */
  def id[I]: Process[I,I] = lift(identity)

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] = (p1, p2) match {
    case (Halt(), _) => Halt()
    case (_, Halt()) => Halt()
    case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
    case (Await(recv1), _) => Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
    case (_, Await(recv2)) => Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))

  }

  def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] = p match {
    case Halt() => p
    case Emit(h, t) => Emit(h, feed(oa)(t))
    case Await(recv) => recv(oa)
  }

  import scala.language.implicitConversions
  implicit def processMonad[I]: Monad[({ type f[x] = Process[I, x]})#f] = new Monad[({ type f[x] = Process[I, x]})#f] {
    def unit[O](o: => O): Process[I, O] = Emit(o)
    def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
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

  def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

  def any: Process[Boolean, Boolean] = loop(false)((b: Boolean, s) => (s || b, s || b))

  def existsResult[I](f: I => Boolean): Process[I, Boolean] =
    exists(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

  def takeThrough[I](f: I => Boolean): Process[I, I] = takeWhile(f) ++ echo

  def echo[I]: Process[I, I] = await(i => emit(i))

  def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
    @tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next())) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines(), p, z)
    finally s.close
  }

  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  def convertFahrenheit: Process[String, Double] = filter((line: String) => !line.startsWith("#")) |>
    filter(line => line.trim.nonEmpty) |>
    lift(line => toCelsius(line.toDouble))
}
