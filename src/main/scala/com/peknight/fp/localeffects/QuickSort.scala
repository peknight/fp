package com.peknight.fp.localeffects

import com.peknight.fp.localeffects.ST.{STArray, STRef}

object QuickSort {

  def swap[S](arr: STArray[S, Int], i: Int, j: Int): ST[S, Unit] = for {
    x <- arr.read(i)
    y <- arr.read(j)
    _ <- arr.write(i, y)
    _ <- arr.write(j, x)
  } yield ()

  def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = {
    for {
      pivotVal <- arr.read(pivot)
      _ <- swap(arr, pivot, r)
      j <- STRef(n)
      _ <- (n until r).foldLeft(ST.noop[S])((s, i) => for {
        _ <- s
        vi <- arr.read(i)
        _ <- if (vi < pivotVal) (for {
          vj <- j.read
          _ <- swap(arr, i, vj)
          _ <- j.write(vj + 1)
        }yield ()) else ST.noop[S]
      } yield ())
      x <- j.read
      _ <- swap(arr, x, r)
    } yield x
  }

  def qs[S](arr: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = if (n < r) { for {
    pi <- partition(arr, n, r, n + (n - r) / 2)
    _ <- qs(arr, n, pi - 1)
    _ <- qs(arr, pi + 1, r)
  } yield () } else ST.noop[S]

  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
  }



  def quicksortOrigin(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    // Partitions a portion of the array into elements less than and greater than pivot, respectively
    def partition(n: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(n: Int, r: Int): Unit = if (n < r) { // Sorts a portion of the array in place
      val pi = partition(n, r, n + (n - r) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length -1)
    arr.toList
  }
}
