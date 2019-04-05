package one.shn.fps.ch6

object Rand {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = ra(rng)
    (f(a), rng2)
  }

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = ra(rng)
    f(a)(rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit[List[A]](Nil))(map2(_, _)(_ :: _))

  def nonNegativeIntLessThan(n: Int): Rand[Int] =
    flatMap(int)(x => if (x > 0 && x < n) unit(x) else nonNegativeIntLessThan(n))

}
