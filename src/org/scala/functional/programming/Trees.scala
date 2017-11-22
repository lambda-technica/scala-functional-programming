package org.scala.functional.programming

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeManipulation {

  def size[A]( t: Tree[A]) : Int = t match {

    case Leaf(_)=>  1
    case Branch(l, r) => size(l) + size(r) + 1

    }


}


object Trees extends  App {

  val tree = Branch( Branch( Leaf( 1 ) , Leaf( 2 ) ) , Branch ( Leaf( 3 ) ,  Leaf( 4 ) ) )


  println(s"Tree size : ${TreeManipulation.size(tree)}")

}
