package com.peknight.fp.fpinscala.datastructures

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, f) => 1 + size(l) + size(f)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l + r + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + (l max r))
}
