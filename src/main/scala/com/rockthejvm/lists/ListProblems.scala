package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.jdk.Accumulator
import scala.util.Random

sealed abstract class RList[+T] {
  /**
    * Standard functions
    */
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]

  // the big 3
  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  // sorting the list in the order defined by the ordering object
  def sorted[S >: T](ordering: Ordering[S]): RList[S]

  /**
    * Hard problems
    */
  // sorting the list in the order defined by the Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]

  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]

  def quickSort[S >: T](ordering: Ordering[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = RNil

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = RNil

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = ???

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = ???

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = ???

  // random samples
  override def sample(k: Int): RList[Nothing] = ???

  /**
    * Hard problems
    */
  // sorting
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def sorted[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(index: Int): T = {
    @tailrec
    def applyTailRec(remaining: RList[T], count: Int): T =
      if (count == index) remaining.head
      else applyTailRec(remaining.tail, count + 1)

    if (index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)

    // Complexity: O(min(N, index))
  }

  // the size of the list
  override def length: Int = {
    @tailrec
    def lengthTailRec(remaining: RList[T], count: Int): Int =
      if (remaining.isEmpty) count
      else lengthTailRec(remaining.tail, count + 1)

    lengthTailRec(this, 0)

    // Complexity: O(N)
  }

  // reverse this list into a new list
  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remaining: RList[T], accumulator: RList[T]): RList[T] =
      if (remaining.isEmpty) accumulator
      else reverseTailRec(remaining.tail, remaining.head :: accumulator)

    reverseTailRec(this, RNil)

    // Complexity: O(N)
  }

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def appendTailRec(remaining: RList[S], accumulator: RList[S]): RList[S] =
      if (remaining.isEmpty) accumulator.reverse
      else appendTailRec(remaining.tail, remaining.head :: accumulator)

    appendTailRec(anotherList, this.reverse)

    // Complexity: O(M+N)
    // length of this list = M
    // length of the other list = N
  }

  // remove an element
  override def removeAt(index: Int): RList[T] = {
    //    @tailrec
    //    def removeAtTailRec(remaining: RList[T], i: Int, accumulator: RList[T]): RList[T] =
    //      if (remaining.isEmpty) accumulator.reverse
    //      else if (i == index) removeAtTailRec(remaining.tail, i + 1, accumulator)
    //      else removeAtTailRec(remaining.tail, i + 1, remaining.head :: accumulator)
    //
    //    if (index < 0) this
    //    else removeAtTailRec(this, 0, RNil)

    // Even Better
    @tailrec
    def removeAtTailrec(remaining: RList[T], currentIndex: Int, accumulator: RList[T]): RList[T] = {
      if (currentIndex == index) accumulator.reverse ++ remaining.tail
      else if (remaining.isEmpty) accumulator.reverse
      else removeAtTailrec(remaining.tail, currentIndex + 1, remaining.head :: accumulator)
    }

    if (index < 0) this
    else removeAtTailrec(this, 0, RNil)

    // Complexity: O(N)
  }

  // the big 3
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(remaining: RList[T], accumulator: RList[S]): RList[S] =
      if (remaining.isEmpty) accumulator.reverse
      else mapTailRec(remaining.tail, f(remaining.head) :: accumulator)

    mapTailRec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailRec(remaining: RList[T], accumulator: RList[S]): RList[S] =
      if (remaining.isEmpty) accumulator.reverse
      else flatMapTailRec(remaining.tail, f(remaining.head).reverse ++ accumulator)

    flatMapTailRec(this, RNil)
  }

  override def filter(predicate: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remaining: RList[T], accumulator: RList[T]): RList[T] =
      if (remaining.isEmpty) accumulator.reverse
      else if (predicate(remaining.head)) filterTailRec(remaining.tail, remaining.head :: accumulator)
      else filterTailRec(remaining.tail, accumulator)

    filterTailRec(this, RNil)
  }

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(T, Int)] = {

    @tailrec
    def rleTailRec(remaining: RList[T], current: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] =
      if (remaining.isEmpty) (current :: accumulator).reverse
      else if (current._1 == remaining.head) rleTailRec(remaining.tail, (current._1, current._2 + 1), accumulator)
      else rleTailRec(remaining.tail, (remaining.head, 1), current :: accumulator)

    rleTailRec(this.tail, (this.head, 1), RNil)
  }

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[T] = {

    @tailrec
    def duplicateEachTailRec(remaining: RList[T], count: Int, accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty && count == 0) accumulator.reverse
      else if (count == 0) duplicateEachTailRec(remaining.tail, k, accumulator)
      else duplicateEachTailRec(remaining, count - 1, remaining.head :: accumulator)
    }

    duplicateEachTailRec(this, k, RNil)
  }

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[T] = {

    @tailrec
    def rotateTailRec(k: Int, accumulator: RList[T]): RList[T] = {
      if (this.isEmpty) accumulator
      else if (k == 0) accumulator
      else rotateTailRec(k - 1, accumulator.tail ++ accumulator.head)
    }

    rotateTailRec(k % this.length, RNil)
  }

  // random samples
  override def sample(k: Int): RList[T] = {

    @tailrec
    def sampleTailRec(k: Int, accumulator: RList[T]): RList[T] = {
      if (k < 0) RNil
      else if (k == 0) accumulator
      else {
        val randomIndex = Random.nextInt(this.length)
        val randomElement = this (randomIndex)
        sampleTailRec(k - 1, randomElement :: accumulator)
      }
    }

    sampleTailRec(k, RNil)

    //    def sampleElegant: RList[T] =
    //      RList
    //        .from(1 to k)
    //        .map( _ => Random.nextInt(this.length) )
    //        .map( index => this(index))
    //
    //    if (k < 0) RNil
    //    else sampleElegant
  }

  /**
    * Hard problems
    */
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
      insertSorted(4, [], [1,2,3,5]) =
      insertSorted(4, [1], [2,3,5]) =
      insertSorted(4, [2,1], [3,5]) =
      insertSorted(4, [3,2,1], [5]) =
      [3,2,1].reverse + (4 :: [5]) =
      [1,2,3,4,5]
      Complexity: O(N)
     */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S] ): RList[S] = {
      if (after.isEmpty || ordering.lt(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    /*
  [3,1,4,2,5].sorted = insertSortTailrec([3,1,4,2,5], []) =
    = insertSortTailrec([1,4,2,5], [3])
    = insertSortTailrec([4,2,5], [1,3])
    = insertSortTailrec([2,5], [1,3,4])
    = insertSortTailrec([5], [1,2,3,4])
    = insertSortTailrec([], [1,2,3,4,5])
    = [1,2,3,4,5]
    Complexity: O(N^2)
    */
    @tailrec
    def insertionSortTailRec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator
      else insertionSortTailRec(remaining.tail, insertSorted(remaining.head, RNil, accumulator))
    }

    insertionSortTailRec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = ???

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = ???
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def convertToRListTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailrec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailrec(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions() = {
    // test get-kth
    println(aSmallList.apply(0))
    println(aSmallList.apply(2))
    println(aLargeList.apply(8735))

    // test length
    println(aSmallList.length)
    println(aLargeList.length)

    // test reverse
    println(aSmallList.reverse)
    println(aLargeList.reverse)

    // test concat
    println(aSmallList ++ aLargeList)

    // test removeAt
    println(aLargeList.removeAt(13))

    // map
    println(aLargeList.map(x => 2 * x))
    // flatMap
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 1.3 seconds!
    println(System.currentTimeMillis() - time)
    // filter
    println(aLargeList.filter(x => x % 2 == 0))
  }

  def testMediumDifficultyFunctions() = {
    // run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)

    // duplicateEach
    println(aSmallList.duplicateEach(4))

    // rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

    // random samples
    println(aLargeList.sample(10))

    // better flatMap
    println(aSmallList.flatMap(x => x :: (2 * x) :: RNil))
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 7 ms
    println(System.currentTimeMillis() - time)
  }

  def testHardFunctions() = {
    val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
    val ordering = Ordering.fromLessThan[Int](_ < _)
    val listToSort = aLargeList.sample(10)

    // insertion sort
    println(anUnorderedList.insertionSort(ordering))
    println(listToSort.insertionSort(ordering))
    // merge sort
    println(listToSort.mergeSort(ordering))
    // quick sort
    println(listToSort.quickSort(ordering))
  }

  testHardFunctions()
}
