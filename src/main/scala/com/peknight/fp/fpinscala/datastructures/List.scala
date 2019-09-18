package com.peknight.fp.fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else (Cons(as.head, apply(as.tail: _*)))

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def map[A, B](as: List[A])(f: A => B) = foldRight(as, Nil: List[B])((a, l) => Cons(f(a), l))

  def map_1[A, B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, Nil: List[B])((a, l) => Cons(f(a), l))

  def map_2[A, B](as: List[A])(f: A => B): List[B] = {
    val buf = ListBuffer.empty[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, l) => append(f(a), l))

  def flatMap_1[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))


  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, l) => if (f(a)) Cons(a, l) else l)

  def filter_1[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, Nil: List[A])((a, l) => if (f(a)) Cons(a, l) else l)

  def filter_2[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = ListBuffer.empty[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b)(_ + _)

  def take[A](as: List[A], n: Int): List[A] = {
    val buf = ListBuffer.empty[A]
    def go(l: List[A], n: Int): Unit =
      if (n <= 0) Nil
      else l match {
        case Nil => Nil
        case Cons(h, t) => buf += h; go(t, n - 1)
      }
    go(as, n)
    List(buf.toList: _*)
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}

