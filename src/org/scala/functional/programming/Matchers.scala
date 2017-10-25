package org.scala.functional.programming

sealed trait List[+A] {

}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {

    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def add[A](element: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("Empty list")
    case Cons(h, xs) => Cons(element, Cons(h, xs))

  }


  def tail[A](list: List[A]): List[A] = list match {
    case Nil => list
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("Empty list")
    case Cons(_, xs) => Cons(head, xs)
  }

  @annotation.tailrec
  def drop[A](list: List[A], n: Int): List[A] = {

    if (n <= 0) list
    else
      list match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  }

  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] =
    list match {
      case Cons(h, xs) if f(h) => dropWhile(xs)(f)
      case _ => list
    }


  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B
  = list match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  def sumRightFold(list: List[Int]): Int = {
    foldRight(list, 0)(_ + _)
  }


  def length(list: List[Int]): Int = {

    foldRight(list, 0)((_, a) => a + 1)

  }

  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((acc, h) => Cons(h, acc))
  }

  def append[A](element: A, list: List[A]): Unit = {




  }

}


object Matchers extends App {


  println(s"Tail ${List(3, 2, 4)}")

  println(s"Tail empty : ${List.tail(List())}")

  println(s"SetHead ${List.setHead(3, List(1, 6, 2, 4))}")

  println(s"Drop 3 ${List.drop(List(1, 2, 3, 4), 3)}")

  println(s"Drop while less than 3 :${List.dropWhile(List(1, 2, 3, 4))((x: Int) => x < 3)}")

  println(s"Fold right multiply ${List.foldRight(List(1, 2, 3, 4), 1)((x: Int, y: Int) => x * y)}")

  println(s"Fold left multiply ${List.foldLeft(List(1, 2, 3, 4), 1)((x: Int, y: Int) => x * y)}")

  println(s"Sum ${List.sumRightFold(List(1, 2, 3, 4))}")

  println(s"Length  ${List.length(List(1, 2, 3, 4))}")

  println(s"Reverse  ${List.reverse(List(1, 2, 3, 4))}")

  println(s"Add  ${List.add(40, List(1, 2, 3, 4))}")
}
