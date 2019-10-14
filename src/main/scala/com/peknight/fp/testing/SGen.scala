package com.peknight.fp.testing

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_) map f)

   def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => {
     forSize(n) flatMap { f(_).forSize(n) }
   })

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))
}
