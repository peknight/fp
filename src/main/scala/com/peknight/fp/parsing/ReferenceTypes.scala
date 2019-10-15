package com.peknight.fp.parsing

object ReferenceTypes {

  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  /* Likewise, we define a few helper functions on `Result`. */
  sealed trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }
    /* Used by `attempt`. */
    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }
    /* Used by `flatMap` */
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e,c) => Failure(f(e),c)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
   * first index where the two strings differed. If s2 is
   * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }
}
