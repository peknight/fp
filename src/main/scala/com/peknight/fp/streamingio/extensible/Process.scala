package com.peknight.fp.streamingio.extensible
import Process._
import com.peknight.fp.iomonad.IO
import com.peknight.fp.monad.Monad

import scala.annotation.tailrec

trait Process[F[_], O] {
  /*
   * Many of the same operations can be defined for this generalized
   * `Process` type, regardless of the choice of `F`.
   */

  def map[O2](f: O => O2): Process[F,O2] = this match {
    case Await(req, recv) =>
      Await(req, recv andThen (_ map f))
    case Emit(h, t) => Try { Emit(f(h), t map f) }
    case Halt(err) => Halt(err)
  }

  def ++(p: => Process[F,O]): Process[F,O] =
    this.onHalt {
      case End => Try(p) // we consult `p` only on normal termination
      case err => Halt(err)
    }

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  /*
   * Anywhere we _call_ `f`, we catch exceptions and convert them to `Halt`.
   * See the helper function `Try` defined below.
   */
  def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] =
    this match {
      case Halt(err) => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
    }

  def repeat: Process[F,O] = this ++ this.repeat

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case Await(req, recv) => F.flatMap(F.attempt(req)) {e => go(Try(recv(e)), acc)}
      }
    go(this, IndexedSeq())
  }

  def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt{
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }
}
object Process {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception
  case object Kill extends Exception

  /**
   * Helper function to safely produce `p`, or gracefully halt
   * with an error if an exception is thrown.
   */
  def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def await[F[_], A,O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    @tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(IO.run(req)))
            catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
      }
    try go(src, IndexedSeq())
    finally E.shutdown
  }

  trait MonadCatch[F[_]] extends Monad[F] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
  }

  def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
    await[IO, R, O](acquire)(r => use(r).onComplete(release(r)))
}
