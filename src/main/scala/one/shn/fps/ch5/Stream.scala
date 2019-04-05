package one.shn.fps.ch5

import scala.language.postfixOps

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Cons(head, _) => Some(head())
    case _             => None
  }

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(head, tail) => go(tail(), head() :: acc)
      case _                => acc
    }
    go(this, Nil) reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 0 => Stream.cons(head(), tail() take n - 1)
    case _                         => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail() drop n - 1
    case _                      => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(head, tail) => p(head()) || (tail() exists p)
    case _                => false
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Cons(head, tail) => p(head()) && (tail() forall p)
    case _                => true
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case _                => z
  }

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Cons(head, tail) => tail().foldLeft(f(z, head()))(f)
    case _                => z
  }

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, s) => Stream.cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def find(p: A => Boolean): Option[A] = this filter p headOption

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, s) => Stream.cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, s) => f(a) append s)

  def tails: Stream[Stream[A]] = Stream.cons(this, tails map {
    case Cons(_, tail) => tail()
    case _             => Stream.empty[A]
  })

}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }
  def apply[A](as: A*): Stream[A] = if (as isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
