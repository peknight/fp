package com.peknight.fp.fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val result@(value, nextRng) = rng.nextInt
    if (value >= 0) result
    else (-(value + 1), nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, nextRng) = nonNegativeInt(rng)
    (value / (Int.MaxValue + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue, rng2) = rng.nextInt
    val (doubleValue, rng3) = double(rng)
    ((intValue, doubleValue), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intValue, doubleValue), rng2) = intDouble(rng)
    ((doubleValue, intValue), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0) (xs, r)
      else {
        val (x, r1) = r.nextInt
        go(count - 1, r1, x :: xs)
      }

    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = state => {
    val (a, state1) = s(state)
    (f(a), state1)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((a, z) => map2(a, z)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
    }
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
