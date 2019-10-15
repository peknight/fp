package com.peknight.fp.parsing

import com.peknight.fp.parsing.ReferenceTypes._
import scala.language.implicitConversions

import scala.util.matching.Regex

object Reference extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val s0 = ParseState(Location(input))
    p(s0).extract
  }

  override implicit def string(s: String): Parser[String] = (input: ParseState) => {
    val i = firstNonmatchingIndex(input.loc.input, s, input.loc.offset)
    if (i == -1) Success(s, s.length)
    else Failure(input.loc.advanceBy(i).toError("'" + s + "'"), i != 0)
  }

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = (s: ParseState) => {
    p(s) match {
      case Success(_, n) => Success(s.slice(n), n)
      case f@Failure(_, _) => f
    }

  }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = (s: ParseState) => {
    s1(s) match {
      case Failure(e, false) => s2(s)
      case r => r
    }
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = (s: ParseState) => {
    p(s) match {
      case Success(a, n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e@Failure(_, _) => e
    }
  }

  override implicit def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    (s: ParseState) => r.findPrefixOf(s.input) match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = (s: ParseState) => {
    p(s).mapError(_.label(msg))
  }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = (s: ParseState) => {
    p(s).mapError(_.push(s.loc, msg))
  }

  override def attempt[A](p: Parser[A]): Parser[A] = (s: ParseState) => p(s).uncommit

  def fail[A](msg: String): Parser[A] = s => Failure(s.loc.toError(msg), true)

  override def many[A](p: Parser[A]): Parser[scala.List[A]] = (s: ParseState) => {
    val nConsumed: Int = 0
    val buf = new collection.mutable.ListBuffer[A]
    def go(p: Parser[A], offset: Int): Result[List[A]] = {
      p(s.advanceBy(offset)) match {
        case Success(a, n) => buf += a; go(p, offset + n)
        case f@Failure(e, true) => f
        case Failure(e, _) => Success(buf.toList, offset)
      }
    }
    go(p, 0)
  }
}
