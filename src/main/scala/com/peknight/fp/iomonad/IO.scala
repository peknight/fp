package com.peknight.fp.iomonad

import com.peknight.fp.monad.Monad

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
}
object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
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
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

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

//  def factorial(n: Int): IO[Int] = for {
//    acc <- ref(1)
//    _ <- foreachM {1 to n to(LazyList)} ((i: Int) => skip(acc.modify(_ * i)))
//    result <- acc.get
//  } yield result
//
//  val factorialREPL: IO
}
