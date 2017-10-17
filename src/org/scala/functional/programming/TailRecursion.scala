package org.scala.functional.programming

import scala.annotation.tailrec

object TailRecursion extends App{

  //https://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def factorialTail(n: Int): Int = {
    @tailrec
    def process(n: Int, accumulator: Int) : Int = {
      if( n<=0)
        accumulator
      else
        process(n-1 , n*accumulator)
    }

    process(n,1)
  }

  def factorial(n: Int): Int = {
    def process(n: Int, accumulator: Int) : Int = {
      if( n<=0)
        accumulator
      else
        process(n-1 , n*accumulator)
    }

    process(n,1)
  }

  val f = time {factorial(100)}
  val ft = time {factorialTail(100)}



}
