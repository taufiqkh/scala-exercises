package fpinscala.datastructures

import scala.annotation.tailrec

/**
  * Functional Programming in Scala
  * 3.1 Defining data structures
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRightX[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRightX(xs, z)(f))
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((len, _) => len + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((t, h) => Cons(h, t))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = drop(1, list)

  @tailrec
  def drop[A](n: Int, list: List[A]): List[A] = n match {
    case 0 => list
    case _ => list match {
      case Nil => throw new IllegalArgumentException("Cannot drop from empty list")
      case Cons(x, xs) => drop(n - 1, xs)
    }
  }

  def dropWhile[A](list: List[A], pred: (A) => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (pred(x)) dropWhile(xs, pred)
      else list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs match {
      case Nil => Nil
      case _ => Cons(x, init(xs))
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def flatten[A](a1: List[List[A]]): List[A] =
    foldRight(a1, Nil:List[A])(append)

  def flattenx[A](a1: List[List[A]]): List[A] =
    reverse(
      foldLeft(a1, Nil:List[A])(
        (all, list) => foldLeft(list, all)((b, a) => Cons(a, b))
      )
    )

  def inc(ints: List[Int]): List[Int] = foldRight(ints, Nil:List[Int])((a, b) => Cons(a + 1, b))

  def stringify(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((d, z) => Cons(d.toString, z))

  def head[A](list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException("Cannot get head of empty list")
    case Cons(x, xs) => x
  }

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a, b) => Cons(f(a), b))

  def setHead[A](head: A, list: List[A]): List[A] = Cons(head, list)
}