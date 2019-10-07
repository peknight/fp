package com.peknight.fp.fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, LinkedBlockingDeque, ThreadPoolExecutor, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def isCancelled: Boolean = false
    def isDone: Boolean = true
    def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val (af, bf) = (pa(es), pb(es))
    Map2Future(af, bf, f)
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def isDone: Boolean = cache.isDefined

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  /*
   * This is the simplest and most natural implementation of `fork`, but there are some problems with it -- for one,
   * the outer Callable will block waiting for the "inner" task to complete. Since this blocking occupies a thread in
   * our thread pool, or whatever resource backs the ExecutorService, this implies that we're losing out on some
   * potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more
   * serious problem with the implementation that we'll discuss later in the chapter.
   */
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = ???
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
}
object ParTest extends App {
  Par.run(new ThreadPoolExecutor(5, 5, 60, TimeUnit.SECONDS,
    new LinkedBlockingDeque[Runnable](10)))(Par.fork(Par.unit{
    Thread.sleep(1000)
    println("task in: " + Thread.currentThread().getName())
  }))
}
