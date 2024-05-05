package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(x) => Some(f(x))

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(x) => x

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None => ob
    case Some(x) => Some(x)

  // or else 2
  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match
    case None => None
    case Some(x) => if f(x) then Some(x) else None

  def filter2(f: A => Boolean): Option[A] =
    flatMap(x => if f(x) then Some(x) else None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(value => Math.pow(value - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  // map2 using flatmap
  def map2WithFlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.flatMap(bb => Some(f(aa, bb))))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((h, acc) => map2(f(h), acc)(_ :: _));

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)
