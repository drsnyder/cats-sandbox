package com.github.drsnyder.monoid

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object Boolean {
    val booleanAndMonoid = new Monoid[Boolean] {
        override def combine(x: Boolean, y: Boolean): Boolean = x && y
        def empty: Boolean = true // so we satisfy equality
    }

    val booleanOrMonoid = new Monoid[Boolean] {
        override def combine(x: Boolean, y: Boolean): Boolean = x || y
        def empty: Boolean = false // so we satisfy equality
    }

    val booleanXOrMonoid = new Monoid[Boolean] {
        override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
        def empty: Boolean = false // so we satisfy equality
    }

    val booleanXNOrMonoid = new Monoid[Boolean] {
        override def combine(x: Boolean, y: Boolean): Boolean =
            (!x || y) && (x || !y)
        def empty: Boolean = true // so we satisfy equality
    }

    def testLaws() {
        assert(Laws.associativeLaw(true, false, booleanAndMonoid.empty)(booleanAndMonoid))
        assert(Laws.identityLaw(true)(booleanAndMonoid))
        assert(Laws.identityLaw(false)(booleanAndMonoid))

        assert(Laws.associativeLaw(true, false, booleanOrMonoid.empty)(booleanOrMonoid))
        assert(Laws.identityLaw(true)(booleanOrMonoid))
        assert(Laws.identityLaw(false)(booleanOrMonoid))

        assert(Laws.associativeLaw(true, false, booleanXOrMonoid.empty)(booleanXOrMonoid))
        assert(Laws.identityLaw(true)(booleanXOrMonoid))
        assert(Laws.identityLaw(false)(booleanXOrMonoid))

        assert(Laws.associativeLaw(true, false, booleanXNOrMonoid.empty)(booleanXNOrMonoid))
        assert(Laws.identityLaw(true)(booleanXNOrMonoid))
        assert(Laws.identityLaw(false)(booleanXNOrMonoid))
    }
}