package fpinscala.laziness

/**
  * Created by Taufiq on 25/11/2016.
  */
import Stream._

import scala.annotation.tailrec
sealed trait Stream[+A] {
  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def headOption2: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => Empty
  }

  def takeWhile(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)

  def takeWhile2(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      if (f(h())) Cons(h, () => t().takeWhile2(f))
      else Empty
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => b.append(f(a)))

  @tailrec
  final def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    case _ => z
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  def constant1[A](a: A): Stream[A] = cons(a, constant1(a))

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  val ones: Stream[Int] = constant(1)

  def from1(n: Int): Stream[Int] = cons(n, from1(n + 1))

  def from(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))

  val fibs1: Stream[Int] = {
    def fibs1(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, fibs1(f1, f0 + f1))
    }
    fibs1(0, 1)
  }

  def fibs: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1)))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}