package com.peknight.fp.parsing

import scala.language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array: Parser[JArray] = surround("[", "]")(value sep "," map {
      vs => JArray(vs.to(IndexedSeq))
    }).scope("array")
    def obj: Parser[JObject] = surround("{", "}")(keyval sep "," map {
      kvs => JObject(kvs.to(Map))
    }).scope("object")
    def keyval: Parser[(String, JSON)] = escapedQuoted ** (":" *> value)
    def lit: Parser[JSON] = {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    } scope ("literal")
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}
