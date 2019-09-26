package com.peknight.fp.fpinscala.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List.empty[A]).reverse
  }

  def toListFast: List[A] = {
    val buf = ListBuffer.empty[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buf += h(); go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case s => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h())) => Some((h(), t()))
    case _ => None
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (h, t) =>
    if (p(h)) cons(h, t)
    else empty
  }

  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>: A](b: => Stream[B]): Stream[B] = foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty[B]))
    case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty[A], t2()))
    case _ => None
  }

  def startsWith[A](prefix: Stream[A]): Boolean = zipAll(prefix).takeWhile(_._2.isEmpty).forAll {
    case (h1, h2) => h1 == h2
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some((s, t()))
    case Empty => None
  }.append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = foldRight((z, Stream(z))){
    case (a, (b, bs)) => {
      val b1 = f(a, b)
      (b1, cons(b1, bs))
    }
  }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }
  }

  def fib: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  def fibViaUnfold: Stream[Int] = {
    unfold[Int, (Int, Int)]((0, 1)) {
      case (value0, value1) => Option((value0, (value1, value0 + value1)))
    }
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some(s, s + 1))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(Some(a, _))

  def startsWith[A](s: Stream[A], prefix: Stream[A]): Boolean = (s, prefix) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => startsWith(t1(), t2())
    case _ => false
  }


  @tailrec
  def hasSubsequence[A](sup: Stream[A], sub: Stream[A]): Boolean = sup match {
    case Empty => sub == Empty
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t(), sub)
  }
}
