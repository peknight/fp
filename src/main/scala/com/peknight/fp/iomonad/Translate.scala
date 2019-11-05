package com.peknight.fp.iomonad

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]
}
