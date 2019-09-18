# Note

## Functional Programming in Scala
[fpinscala github repo](https://github.com/fpinscala/fpinscala.git "Functional Programming in Scala's github repository")  

## Scala With Cats
[Scala with Cats Book](https://underscore.io/books/scala-with-cats/)

### The implicitly Method

The Scala standard library provides a generic type class interface called `implicitly`. We can use `implicitly` to 
summon any value from implicit scope: 

```scala
def implicitly[A](implicit value: A): A = value
```

### Show

[`cats.Show`](https://typelevel.org/cats/api/cats/Show.html)

`Show` provides a mechanism for producint developer-friendly console output without using toString.

```scala
package cats

trait Show[A] {
  def show(value: A): String
}

object Show {
  // Convert a function to a `Show` instance:
  def show[A](f: A => String): Show[A] = ???
  
  // Create a `Show` instance from a `toString` method
  def fromToString[A]: Show[A] = ???
  
}
```

Usage:

```scala
import cats.Show
import cats.syntax.show._ // for show
```

### Eq

[`cats.Eq`](https://typelevel.org/cats/api/cats/kernel/Eq.html)

`Eq` is designed to support type-safe equality and address annoyances using Scala's built-in == operator.

```scala
package cats

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
  // other concrete methods based on eqv...
}
```

The interface syntax, defined in `cats.syntax.eq`, provides two methods for performing equality checks provided there is 
an instance Eq[A] in scope:

* `===` compares two objects for equality;
* `=!=` compares two objects for inequality;

Usage:

```scala
import cats.Eq
import cats.syntax.eq._ // for === =!=
```

We can define our own instances of `Eq` using the `Eq.instance[A]` method, which accepts a function of type 
`(A, A) => Boolean` and returns an `Eq[A]`.

### Option

We can using the `Option.apply` and `Option.empty` methods from the standard library:

```scala
Option(1)
Option.empty[Int]
```

or using special syntax from `cats.syntax.option`:

```scala
import cats.syntax.option._

1.some
none[Int]
```

### Monoid

[`cats.Monoid`](https://typelevel.org/cats/api/cats/kernel/Monoid.html)

Formally, a monoid for a type `A` is:

* an operation `combine` with type `(A, A) => A`
* an element `empty` of type `A`

Here is a simplified version of the difinition from Cats:

```scala
trait Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}
```

> *Monoid Laws*
>
> Monoids must formally obey several *laws*. For all values `x`, `y`, and `z`, 
> in `A`, `combine` must be associative and `empty` must be an identity element:
> 
> ```
> def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
>   m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
> }
> 
> def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
>   (m.combine(x, m.empty) == x) && (m.combine(m.emtpy, x) == x)
> }
> ```

Cats provides syntax for the `combine` method in the form of the `|+|` operator.

Usage:

```scala
import cats.Monoid
import cats.syntax.semigroup._ // for |+|
```

### Semigroup

[`cats.Semigroup`](https://typelevel.org/cats/api/cats/kernel/Semigroup.html)

A semigroup is just the `combine` part of a monoid.  
A more accurate (though still simplified) definition of Cats' `Monoid` is:

```scala
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
```

Usage:

```scala
import cats.Semigroup
import cats.syntax.semigroup._ // for |+|
```

### Functor

[`cats.Functor`](http://typelevel.org/cats/api/cats/Functor.html)

Informally, a functor is anything with a `map` method.  
Here's a simplified version of the definition:

```scala
package cats
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

> *Functor Laws*
>
> Identity: call `map` with the identity function is the same as doing nothing:
> ```
> fa.map(a => a) == fa
> ```
> Composition: `mapping` with two functions `f` and `g` is the same as `mapping` with `f` and then `mapping` with `g`:
> ```
> fa.map(g(f(_))) == fa.map(f).map(g)
> ```

We obtain instances using the standard `Functor.apply` method on the companion object.

`Functor` provides the `lift` method, which converts a function of type `A => B` to one that operates over a functor and 
has type `F[A] => F[B]`:

Usage:

```scala
import cats.Functor
import cats.syntax.functor._ // for map
```

### Contravariant

[`cats.Contravariant`](http://typelevel.org/cats/api/cats/Contravariant.html)

The *contravariant functor* provides an operation called `contramap` that 
represents "prepending" and operation to a chain.

```scala
trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
```

We can summon instances of `Contravariant` using the `Contravariant.apply` method.

Usage:
```scala
import cats.Contravariant
import cats.syntax.contravariant._ // for contramap
```

### Invariant

[`cats.Invariant`](https://typelevel.org/cats/api/cats/Invariant.html)

*Invariant functors* implement a method called `imap` that is informally equivalent to a combination of `map` and 
`contramap`. If `map` generates new type class instances by appending a function to a chain, and `contramap` generates 
them by prepending an operation to a chain, `imap` generates them via a pair of bidirectional transformations.

```scala
trait Invariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}
```
Usage:
```scala
import cats.Invariant
import cats.syntax.invariant._ // for imap
```

Cats provides an instance of `Invariant` for `Monoid`.  
Imagine we want to produce a `Monoid` for Scala's `Symbol` type:

```scala
import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap

implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)
```


## Monocle

[Monocle](http://julien-truffaut.github.io/Monocle/) Optics library for Scala

