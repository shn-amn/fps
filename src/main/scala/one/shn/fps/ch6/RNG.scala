package one.shn.fps.ch6

import one.shn.fps.ch5.Stream

import scala.language.postfixOps

trait RNG {
  def nextInt: (Int, RNG)
  def ints(count: Int): (List[Int], RNG) = {
    lazy val s: Stream[(Int, RNG)] = Stream.cons(this.nextInt, s map { case (_, rng) => rng.nextInt })
    val (ints, rngs) = s.take(count).toList.unzip
    (ints, rngs.reverse.headOption getOrElse this)
  }
}
