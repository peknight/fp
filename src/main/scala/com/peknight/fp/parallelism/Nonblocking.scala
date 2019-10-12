package com.peknight.fp.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {
  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] = _ => (cb: A => Unit) => cb(a)

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] = _ => (cb: A => Unit) => cb(a)

    def fork[A](a: => Par[A]): Par[A] = es => (cb: A => Unit) => eval(es)(a(es)(cb))

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    private[parallelism] def async[A](f: (A => Unit) => Unit): Par[A] = _ => (cb: A => Unit) => f(cb)

    /**
     * Helper function, for evaluating an action asynchronously, using the given `ExecutorService`.
     */
    private[parallelism] def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p1(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = es => (cb: B => Unit) => pa(es)(a => eval(es)(cb(f(a))))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    private[parallelism] def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

    def parMap[A, B](ps: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = sequenceBalanced(ps.map(asyncF(f)))

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = as.map (asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => (cb: B => Unit) =>
      pa(es)(a => eval(es)(f(a)(es)(cb)))

    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond){ if(_) t else f }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = flatMap(key)(choices(_))

    def join[A](a: Par[Par[A]]): Par[A] = es => (cb: A => Unit) => a(es)(p => eval(es)(p(es)(cb)))

    def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = map2(p1, p2)(_ == _)

    /* Gives us infix syntax for `Par`. */
    import scala.language.implicitConversions
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }
}

