package com.peknight.fp.iomonad

import com.peknight.fp.iomonad.Free.Suspend
import com.peknight.fp.iomonad.Translate.~>
import com.peknight.fp.monad.Monad
import com.peknight.fp.parallelism.Nonblocking.Par

import scala.io.StdIn._

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
  def toState: ConsoleState[A]
}
object Console {

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run
    def toReader = ConsoleReader(in => Some(in))
    def toState = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }

    def run: Option[String] = try Some(readLine()) catch { case e: Exception => None}
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
    def toReader = ConsoleReader(_ => ())
    def toState = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) } // append to the output
  }

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar = new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }
  val consoleToReader = new (Console ~> ConsoleReader) { def apply[A](a: Console[A]) = a.toReader }
  val consoleToState = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]) = a.toState
  }

  import scala.language.implicitConversions
  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: () => A)(f: A => () => B) = () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  /**
   * isn't stack-safe, since flatMap isn't stack-safe for Function0 (it has the same problem as our original, naive
   * IO type in which run called itself in the implementation of flatMap)
   * @param a
   * @tparam A
   * @return
   */
  def runConsoleFunction0[A](a: Free[Console, A]): () => A = Free.runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] = Free.runFree[Console, Par, A](a)(consoleToPar)

  def runConsole[A](a: Free[Console, A]): A = Free.runTrampoline { Free.translate(a)(consoleToFunction0)}

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] = Free.runFree[Console, ConsoleReader, A](io)(consoleToReader)

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] = Free.runFree[Console, ConsoleState, A](io)(consoleToState)

  def nonblockingRead(source: Source, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = Par.async {
    (cb: Either[Throwable, Array[Byte]] => Unit) => source.readBytes(numBytes, cb)
  }

  def readPar(source: Source, numBytes: Int): Free[Par, Either[Throwable, Array[Byte]]] = Suspend(nonblockingRead(source, numBytes))

  import java.nio._
  import java.nio.channels._
  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = {
    Par.async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      val buf = ByteBuffer.allocate(numBytes)
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit] {
        def completed(bytesRead: Integer, ignore: Unit) = {
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr))
        }
        def failed(err: Throwable, ignore: Unit) = cb(Left(err))
      })
    }
  }
}
