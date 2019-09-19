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

### Monad

[`cats.Monad`](https://typelevel.org/cats/api/cats/Monad.html)

Informally, a monad is anything with a constructor and a `flatMap` method. 

> A monad is a mechanism for sequencing computations.

Here is a simplified version of the `Monad` type class in Cats:

```scala
trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
}
```
> *Monad Laws*
>
> *Left identity*: calling `pure` and transforming the result with `func` is the same as calling `func`:
> ```
> pure(a).flatMap(func) == func(a)
> ```
> *Right identity*: passing `pure` to `flatMap` is the same as doing nothing:
> ```
> m.flatMap(pure) == m
> ```
> *Associativity*: `flatMapping` over two functions `f` and `g` is the same as `flatMapping` over `f` and then 
`flatMapping` over `g`:
> ```
> m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
> ```

Every monad is also a functor:

```
def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
```

`Monad` extends two other type classes: `FlatMap`, which provides the `flatMap` method, and `Applicative`, which 
provides `pure`. `Applicative` also extends `Functor`.

```scala
import cats.Monad
import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map
import cats.syntax.applicative._ // for pure
```

### Id

`cats.Id`

Here is the definition of `Id` to explain:

```scala
type Id[A] = A
```

Cats provides instances of various type classes for `Id`, including `Functor` and `Monad`.

### Either

```scala
import cats.syntax.either._ // for asLeft asRight
```

`cats.syntax.either` adds some useful extension methods to the `Either` companion object. The `catchOnly` and 
`catchNonFatal` methods are great for capturing `Exceptions` as instances of `Either`:

```
Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Badness"))
```

There are also methods for creating an `Either` from other data types:

```
Either.fromTry(scala.util.Try("foo".toInt))
Either.fromOption[String, Int](None, "Badness") // res3: Either[String, Int] = Left(Badness)
```

We can use `orElse` and `getOrElse` to extract values from the right side or return a default:

```
"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])
```

The `ensure` method allows us to check whether the right-hand value satisfies a predicate:

```
-1.asRight[String].ensure("Must be non-negative!")(_ > 0)
```

The `recover` and `recoverWith` methods provide similar error handling to their namesakes on `Future`:

```
"error".asLeft[Int].recover { case str: String => -1 }
"error".asLeft[Int].recoverWith { case str: String => Right(-1) }
```

There are `leftMap` and `bimap` methods to complement `map`:

```
"foo".asLeft[Int].leftMap(_.reverse)
6.asRight[String].bimap(_.reverse, _ * 7)
"bar".asLeft[Int].bimap(_.reverse, _ * 7)
```

The `swap` method lets us exchange left for right:
```
123.asRight[String].swap
```

Finally, Cats adds a host of conversion methods: `toOption`, `toList`, `toTry`, `toValidated`, and so on.

### MonadError

[`cats.MonadError`](https://typelevel.org/cats/api/cats/MonadError.html)

Cats provides an additional type class called `MonadError` that abstracts over `Either`-like data types that are used 
for error handling. `MonadError` provides extra operations for raising and handling errors. 

Here is a simplified version of the definition of `MonadError`:
```
trait MonadError[F[_], E] extends Monad[F] {
  // Lift an error into the `F` context:
  def raiseError[A](e: E): F[A]
  
  // Handle an error, potentially recovering from it:
  def handleError[A](fa: F[A])(f: E => A): F[A]
  
  // Test an instance of `F`,
  // failing if the predicate is not satisfied:
  def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
}
```

In reality, `MonadError` extends another type class called `ApplicativeError`.

`MonadError` is defined in terms of two type parameters:

* `F` is the type of the monad;
* `E` is the type of error contained within `F`.

Here's an example where we instantiate the type class for `Either`:

```scala
import cats.MonadError
import cats.instances.either._ // for MonadError

type ErrorOr[A] = Either[String, A]

val monadError = MonadError[ErrorOr, String]
```

```scala
import cats.syntax.applicative._ // for pure
import cats.syntax.applicativeError._ // for raiseError handleError
import cats.syntax.monadError._ // for ensure
```

Cats provides instances of `MonadError` for numerous data types including `Either`, `Future`, and `Try`. The instance 
for `Either` is customisable to any error type, whereas the instances for `Future` and `Try` always represent errors as
`Throwables`.

### Eval

[`cats.Eval`](https://typelevel.org/cats/api/cats/Eval.html)

`cats.Eval` is a monad that allows us to abstract over different models of evaluation.
`Eval` has three subtypes: `Now`, `Later`, and `Always`. 

We can extract the result of an `Eval` using its `value` method:
```scala
import cats.Eval
val now = Eval.now(math.random + 1000)
val later = Eval.later(math.random + 2000)
val always = Eval.always(math.random + 3000)

now.value
later.value
always.value
```

The three behaviours are summarized below:

| Scala | Cats | Properties |
| :--- | :--- | :--- |
| `val` | `Now` | eager, memoized |
| `lazy val` | `Later` | lazy, memoized |
| `def` | `Always` | lazy, not memoized |

`Eval's` `map` and `flatMap` methods add computations to a chain. In this case, however, the chain is stored explicitly 
as a list of functions. The functions are't run util we call `Eval's` `value` method to request a result.

While the semantics of the originating `Eval` instances are maintained, mapping functions are always called lazily on 
demand (`def` semantics).

`Eval` has a `memoize` method that allows us to memoize a chain of computations. 

`Eval.defer` takes an existing instance of `Eval` and defers its evaluation. The `defer` method is trampolined like 
`map` and `flatMap`:

```scala
import cats.Eval

def factorial(n: BigInt): Eval[BigInt] = 
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }
```

## Monocle

[Monocle](http://julien-truffaut.github.io/Monocle/) Optics library for Scala

