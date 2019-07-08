package com.github.drsnyder

final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

// instances / implementations
object PrintableInstances {
  implicit val stringPrinter: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val intPrinter: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }

  implicit val catPrinter: Printable[Cat] =
    new Printable[Cat] {
      def format(cat: Cat): String = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"${name} is a ${age} year-old ${color} cat."
      }
    }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)
    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(p.format(value))
}

// import com.github.drsnyder._
// implicit val catShow: Show[Cat] =
//   Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")