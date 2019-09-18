package com.peknight.fp.fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def getOrElse[B >: A] (default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def flatMapViaMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case s@Some(_) => s
    case None => ob
  }

  def orElseViaMap[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case s@Some(v) => if (f(v)) s else None
    case None => None
  }

  def filterViaFlatMap(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] = try Some(a) catch { case _: Exception => None }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    sa <- a
    sb <- b
  } yield f(sa, sb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???



}
