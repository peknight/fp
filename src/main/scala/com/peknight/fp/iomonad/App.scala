package com.peknight.fp.iomonad

import com.peknight.fp.parallelism.Nonblocking.Par

abstract class App {
  import java.util.concurrent._
  import Console.parMonad
  import Free.IO
  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A = Par.run(pool)(Free.run(a)(parMonad))

  def main(args: Array[String]): Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args.toIndexedSeq))(pool)
  }

  def pureMain(args: IndexedSeq[String]): IO[Unit]
}
