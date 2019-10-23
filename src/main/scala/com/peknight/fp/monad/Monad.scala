package com.peknight.fp.monad

import com.peknight.fp.testing.Gen

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa) { a => map(fb) { b => f(a, b) } }

  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)((a: F[A]) => a)
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List.empty[B])) { (a: A, flb: F[List[B]]) =>
    map2(f(a), flb)(_ :: _)
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A])) { (a, fla) =>
      map2(f(a), fla) { (flag, la) => if (flag) a :: la else la }
    }
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}
object Monad {
  import com.peknight.fp.testing._
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  import com.peknight.fp.parallelism.Nonblocking._
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  import com.peknight.fp.parsing._
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val lazyListMonad: Monad[LazyList] = new Monad[LazyList] {
    def unit[A](a: => A): LazyList[A] = LazyList(a)
    def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  import com.peknight.fp.state._
  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }
}
