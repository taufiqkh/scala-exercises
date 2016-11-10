package fpinscala.datastructures

/**
  * Chapter 3, exercise 3.5, Functional Programming in Scala
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = fold(tree)((_) => 1)(_ + _)

  def maximum(tree: Tree[Int]): Int = fold(tree)((a) => a)((l, r) => l.max(r))

  def depth[A](tree: Tree[A]): Int = fold(tree)((_) => 0)((l, r) => 1 + l.max(r))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def func(a: A): Tree[B] = Leaf(f(a))
    fold(tree)(func)((l, r) => Branch(l, r))
  }

  def fold[A, B](tree: Tree[A])(f: (A) => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def test[A](tree: Tree[A]): A = tree match {
    case Leaf(value) => value
  }
}