package com.peknight.fp.monoid

import com.peknight.fp.parallelism.Nonblocking._
import com.peknight.fp.testing.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      Prop.forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = v.length
    if (length > 1) {
      val (l, r) = v.splitAt(length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    } else if (v.length == 1) {
      f(v(0))
    } else {
      m.zero
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    Par.flatMap(Par.parMap(v)(f)) { bs => foldMapV(bs, par(m)) { b => Par.lazyUnit(b) } }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  def stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = List.empty[A]
  }

  def intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  def intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  def booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  def booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    val zero = (a: A) => a
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(wc1: WC, wc2: WC): WC = { (wc1, wc2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(s2, i, r)) => Part(s1 + s2, i, r)
      case (Part(l, i, s1), Stub(s2)) => Part(l, i, s1 + s2)
      case (Part(l, i1, s1), Part(s2, i2, r)) => Part(l, i1 + i2 + (if ((s1 + s2).isEmpty) 0 else 1), r)
    }


    }
    val zero = Stub("")
  }
  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString())

    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))
    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B = (a: A) => {
      mb.op(a1(a), a2(a))
    }
    def zero: A => B = _ => mb.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}
