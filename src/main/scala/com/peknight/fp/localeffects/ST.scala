package com.peknight.fp.localeffects

import com.peknight.fp.localeffects.ST.STRef

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}
object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1

  def noop[S] = ST[S, Unit](())

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell = a
    })
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }
    def read(i: Int): ST[S, A] = ST(value(i))
    def freeze: ST[S, List[A]] = ST(value.toList)
    def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](())) {
      case ((key, value), st) => st flatMap (_ => write(key, value))
    }
  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      })

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
  }

  import scala.collection.mutable.HashMap
  sealed trait STMap[S, K, V] {
    protected def table: HashMap[K, V]

    def size: ST[S, Int] = ST(table.size)

    def apply(k: K): ST[S, V] = ST(table(k))

    def get(k: K): ST[S, Option[V]] = ST(table.get(k))

    def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

    def -=(k: K): ST[S, Unit] = ST(table -= k)
  }
  object STMap {
    def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
      val table = HashMap.empty[K, V]
    })

    def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
      val table = (HashMap.newBuilder[K, V] ++= m).result()
    })
  }
}
