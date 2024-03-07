package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
   * which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  // Answer: Case 3rd x + y will be returned: 1 + 2 = 3
  val result: Int = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("Cannot delete element from empty list")
    case Cons(_, y) => y

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("cannot set head on empty list")
    case Cons(_, t) => Cons(h, t)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match
        case Nil => Nil
        case Cons(_, y) => drop(y, n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(x, y) if f(x) => dropWhile(y, f)
    case _ => l


  // My Approach
  def init[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))

  // Answer from book
  private def initAnswer[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("cannot init empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, initAnswer(t))


  def length[A](l: List[A]): Int = foldRight(l, 0, (x, y) => 1 + y)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (acc, x) => acc + x)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, (acc, x) => acc * x)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (h, acc) => Cons(h, acc))

  // ON HOLD
  private def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    reverse(foldLeft(r, reverse(l), (acc, h) => Cons(h, acc)))

  // TODO
  //  def foldRightWithFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  //    foldLeft(reverse(as), acc, (acc, h) => f(h, acc))
  //  def foldLeftWithFoldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  //    foldRight(reverse(as), acc, f)


  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], append)


  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List.Nil, (h, acc: List[Int]) => Cons(h + 1, acc))

  // reverse(foldLeft(l, List.Nil, (acc: List[Int], h) => Cons(h + 1, acc)))
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (h, acc) => Cons(h.toString, acc))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B], (h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (h, acc) => if f(h) then Cons(h, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (h) => if f(h) then Cons(h, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))


  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

// 1,2,3,4  // 2,3
// 1,2
// 2,3

  @tailrec
  def checkIfPrefix[A](a: List[A], b: List[A]): Boolean = (a, b) match
    case (_, List.Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => checkIfPrefix(t1, t2)
    case (_, _) => false


  @tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = a match
    case Nil => b == Nil
    case x if checkIfPrefix(x,b) => true
    case Cons(h,t) => hasSubsequence(t, b)

//  @main
//  def main: Unit =
//    val a = List(1,2,3,5)
//    val b = List(2,5)
//    println(hasSubsequence(a,b))
