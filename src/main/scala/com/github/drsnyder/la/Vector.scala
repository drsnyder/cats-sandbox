package com.github.drsnyder.la


object Vec {
    case class Vec[A](v: Seq[A]) {
        import Number._

        def +(theirs: Vec[A])(implicit number: Number[A]): Either[String, Vec[A]] = {
            if (theirs.v.size != v.size) Left(s"cannot zip with shapes {v.size}, {that.size}")
            else {
                val zipped = v zip theirs.v map { case (a, b) => number.plus(a, b)}
                Right(Vec(zipped))
            }
        }

        def *(theirs: Vec[A])(implicit number: Number[A]): Either[String, Vec[A]] = ???
    }
}

// see https://github.com/typelevel/spire/blob/0ee38b7abc9a42fe92a63c654498305ec80be454/core/src/main/scala/spire/std/array.scala
// https://docs.scala-lang.org/tutorials/FAQ/collections.html
object VectorSyntax {
    // Ignoring size differences for now
    implicit class VectorOps[A](a: Seq[A]) {
        def +:+ (b: Seq[A])(implicit number: Number[A]): Seq[A] =
            a zip b map { case (ae, be) => number.plus(ae, be) }
        def dot (b: Seq[A])(implicit number: Number[A]): Seq[A] =
            a zip b map { case (ae, be) => number.plus(ae, be) }
        def :* (b: A)(implicit number: Number[A]): Seq[A] =
            a map (e => number.multiply(e, b))
        def :+ (b: A)(implicit number: Number[A]): Seq[A] =
            a map (e => number.plus(e, b))
        def :/ (b: A)(implicit number: Number[A]): Seq[A] =
            a map (e => number.divide(e, b))
    }
}