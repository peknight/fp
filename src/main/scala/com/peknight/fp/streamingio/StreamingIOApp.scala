package com.peknight.fp.streamingio

import com.peknight.fp.iomonad.IO
import com.peknight.fp.streamingio.Process.Halt

object StreamingIOApp extends App {
  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next
        count += 1
      }
      count > 40000
    } finally src.close
  }

  def lines(filename: String): IO[LazyList[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.to(LazyList) lazyAppendedAll {src.close; LazyList.empty}
  }

//  val lines: LazyList[String] = LazyList("a", "b", "c")
//
//  lines.zipWithIndex.exists(_._2 + 1 >= 40000)
//
//  lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
//
//  lines.filter(!_.trim.isEmpty).take(40000).map(_.head).indexOfSlice("abracadabra".toList)

  val p = Process.liftOne((x: Int) => x * 2)
  val xs = p(LazyList(1, 2, 3)).toList
  println(xs)

  val x: Process[Int, Int] = Halt().repeat
  println(x)

}
