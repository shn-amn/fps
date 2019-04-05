import one.shn.fps.ch6.SimpleRNG

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, _) = rng2.nextInt
rng ints 12