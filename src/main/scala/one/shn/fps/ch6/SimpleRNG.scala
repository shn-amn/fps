package one.shn.fps.ch6

import scala.language.postfixOps

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    (newSeed >>> 16 toInt, SimpleRNG(newSeed))
  }
}
