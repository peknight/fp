package com.peknight.fp.fpinscala.testing


object TestingApp extends App {

  import org.scalacheck.Gen
  val intList = Gen.listOf(Gen.choose(0, 100))
  import org.scalacheck.Prop._
  val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
    forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
  val failingProp = forAll(intList)(ns => ns.reverse == ns)

  prop.check
  failingProp.check

  println("\n----------\n")

  val intProp = forAll(Gen.choose(0, 200))(_ >= 0)
  intProp.check

  println("\n----------\n")

}
