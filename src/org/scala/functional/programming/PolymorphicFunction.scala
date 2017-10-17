package org.scala.functional.programming

object PolymorphicFunction {

  /**
    * Function checks if array list is ordered by supplied function
    *
    * @param as      - array
    * @param ordered - compare function
    * @tparam A - type of array
    * @return - true if sorted, false if unsorted
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    if (as.isEmpty)
      throw new Exception("Input array is empty")
    else {

      for (Array(left, right) <- as.sliding(2)) {

        if (!ordered(left, right)) {
          return false
        }
      }
      true
    }
  }
}

object PolyMain extends App {

  val arrayOrdered = Array[Int](1, 2, 3, 4, 5)
  val arrayUnOrdered = Array[Int](3, 2, 1, 4, 5)

  val s = PolymorphicFunction.isSorted[Int](arrayOrdered, (x, y) => x < y)

  assert(s)

  val su = PolymorphicFunction.isSorted[Int](arrayUnOrdered, (x, y) => x < y)

  assert(!su)

}
