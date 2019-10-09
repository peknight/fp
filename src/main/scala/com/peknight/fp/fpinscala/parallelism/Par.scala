package com.peknight.fp.fpinscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[+A] {
  private[parallelism] def apply(k: A => Unit): Unit
}
object Par {
  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: => A): Par[A] = _ => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r })

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

  def get[A](a: Par[A]): A = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] = l.foldRight[Par[List[A]]](unit(List()))((h, t) =>
    map2(h, t)(_ :: _)
  )

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

//  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = p1(e).get == p2(e).get
}

object ParTest extends App {
  val S = Executors.newFixedThreadPool(4)
  val echoer = Actor[String](S) {
    msg => println (s"Got message: '$msg'")
  }
  echoer ! "hello"

  Par.run(new ThreadPoolExecutor(5, 5, 60, TimeUnit.SECONDS,
    new LinkedBlockingDeque[Runnable](10)))(Par.fork(Par.unit{
    Thread.sleep(1000)
    println("task in: " + Thread.currentThread().getName())
  }))
}
