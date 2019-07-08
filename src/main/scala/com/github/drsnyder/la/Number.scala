package com.github.drsnyder.la

trait Number[T] {
    def plus(x: T, y: T): T
    def divide(x: T, y: T): T
    def multiply(x: T, y: T): T
    def minus(x: T, y: T): T
}

object Number {
    // can we do one of these for Int, Long, Float, and Double?
    implicit object NumberLikeDouble extends Number[Double] {
        def plus(x: Double, y: Double): Double = x + y
        def multiply(x: Double, y: Double): Double = x * y
        def divide(x: Double, y: Double): Double = x / y
        def minus(x: Double, y: Double): Double = x - y
    }
    implicit object NumberLikeInt extends Number[Int] {
        def plus(x: Int, y: Int): Int = x + y
        def multiply(x: Int, y: Int): Int = x * y
        def divide(x: Int, y: Int): Int = x / y
        def minus(x: Int, y: Int): Int = x - y
    }
}