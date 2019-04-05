package one.shn.fps.ch7

trait Par[+A]

object Par {
  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???
  def map[A, B](a: Par[A])(f: A => B): Par[B] = ???
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = ???
}
