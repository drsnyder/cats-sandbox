package com.github.drsnyder.la

import cats._
import cats.data._
import cats.implicits._


sealed trait Matrix[A] {
  import Number._
  def get: Vector[Vector[A]]
  def +(other: Matrix[A])(implicit ev: Number[A]): Matrix[A]
  def *(other: Int): Matrix[A]
  def *(other: Double): Matrix[A]
  def idx(row: Int, col: Int): Matrix[A]
  def flatMap[A, B](f: A => Matrix[B]): Matrix[B]
  def map[B](f: A => B): Matrix[B]
  def pure[A](a: Vector[A]): Matrix[A]
}

// object MatrixInstances {
//     implicit val MatrixMonad: Monad[Matrix] = new Monad[Matrix] {
//         def flatMap[A, B](fa: Matrix[A])(f: A => Matrix[B]): Matrix[B] = fa.flatMap(f)
//         def pure[A](a: Vector[A]): Matrix[A] = VMatrix(Vector(a))
//     }
// }

object Matrix {

  case class VMatrix[A](rows: Vector[Vector[A]]) extends Matrix[A] {
    import Number._
    def get: Vector[Vector[A]] = rows

    // import MatrixInstances._
    def apply(column: Vector[A]): Matrix[A] =
      // simplification for now
      VMatrix(Vector(column))

    // assumes vectors of the same size
    def +(other: Matrix[A])(implicit ev: Number[A]): Matrix[A] = VMatrix(
      (
        for {
          idx <- (0 until this.rows.size)
          v <- this.get(idx) zip other.get(idx) map (
              (t: (A, A)) => ev.plus(t._1, t._2)
          )
        } yield v
      ).sliding(this.rows.size, this.rows.size)
        .toVector
        .map(Vector.apply)
    )

    def *(other: Int): Matrix[A] = ???
    def *(other: Double): Matrix[A] = ???
    def idx(row: Int, col: Int): Matrix[A] = ???
    def flatMap[A, B](f: A => Matrix[B]): Matrix[B] = ???
    def map[B](f: A => B): Matrix[B] = VMatrix(this.rows map (_ map f))
    def pure[A](a: Vector[A]): Matrix[A] = ???
  }

}
