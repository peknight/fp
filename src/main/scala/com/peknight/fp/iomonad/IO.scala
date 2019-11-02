package com.peknight.fp.iomonad

import com.peknight.fp.iomonad.IO.{FlatMap, Return}
import com.peknight.fp.monad.Monad

import scala.annotation.tailrec

sealed trait IO[A] { self =>
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}
object IO extends Monad[IO] {

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => {
      x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
      }
    }
  }

  def unit[A](a: => A): IO[A] = Suspend(() => a)
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
  sealed class IORef[A](var value: A) {
    def set(a: A): IO[A] = IO { value = a; a }
    def get: IO[A] = IO { value }
    def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
  }

  import scala.io.StdIn.readLine
  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = Suspend(() => Return(println(msg)))

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
  val readInts = map2(readInt, readInt)((_, _))

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  import Player._
  def contest(p1: Player, p2: Player): IO[Unit] = PrintLine(winnerMsg(winner(p1, p2)))
  def empty: IO[Unit] = IO { () }

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM {1 to n to(LazyList): LazyList[Int]} (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val helpstring = """
                     | The Amazing Factorial REPL, v2.0
                     | q - quit
                     | <number> - compute the factorial of the given number
                     | <anything else> - bomb with horrible error
                     """.trim.stripMargin

  val factorialREPL: IO[Unit] = sequence_(
    IO {println(helpstring)},
    doWhile { IO { readLine } } { line =>
      when (line != "q") { for {
        n <- factorial(line.toInt)
        _ <- IO { println("factorial: " + n)}
      } yield () }
    }
  )
}

object IOApp extends App {
  import IO._
  val p = IO.forever(PrintLine("Still going..."))
  run(p)
}
