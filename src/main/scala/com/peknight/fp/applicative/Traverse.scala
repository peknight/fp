package com.peknight.fp.applicative

import com.peknight.fp.monad.{Functor, Monad}

trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)
}
object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f:  A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

}
