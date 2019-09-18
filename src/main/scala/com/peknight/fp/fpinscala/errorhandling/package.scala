package com.peknight.fp.fpinscala

package object errorhandling {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  case class Employee(name: String, department: String)
  def lookupByName(name: String): Option[Employee] = ???
  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
