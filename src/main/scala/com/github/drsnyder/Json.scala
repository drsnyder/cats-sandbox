package com.github.drsnyder

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// type class
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// instance for type class JsonWriter
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
    }

  // enables Json.toJson(Option("A string")) /* implicits (optionWriter(stringWriter)) */
  implicit def optionWriter[A](
      // parameters also have to be implicit
      implicit writer: JsonWriter[A]
  ): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(aValue) => writer.write(aValue)
          case None         => JsNull
        }
    }
  // etc...
}

// interfaces
// Json.toJson(Person("Dave", "dave@example.com")) /* implicit (personWriter) */
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// Extension method which extends the A type; previously called type extension or pimping.
// Person("Dave", "dave@example.com").toJson /* implicit (personWriter) */
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

// summon implicits from the scope
// implicitly[JsonWriter[String]]
