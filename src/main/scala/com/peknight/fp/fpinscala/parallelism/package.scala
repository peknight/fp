package com.peknight.fp.fpinscala

package object parallelism {
  import Par.Par
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)((a, b) => a + b)

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def sumViaPar(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }

  def sumPar(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sumPar(l)), Par.fork(sumPar(r)))(_ + _)
    }

  def maximumPar(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.isEmpty) sys.error("maximum of empty seq")
    else if (ints.size <= 1) Par.unit(ints(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(maximumPar(l)), Par.fork(maximumPar(r)))(_ max _)
    }

  def wordCount(paragraphs: List[String]): Par[Int] = Par.map(Par.sequence(paragraphs.map(s => Par.lazyUnit(s.split("\\s+").size))))(_.sum)
}
