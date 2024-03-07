package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l,r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(i) => f(i)
    case Branch(l, r) => g(l.fold(f,g), r.fold(f,g))

  def sizeViaFold: Int = this.fold(i => 1, _ + _)

  def depthViaFold: Int = this.fold(i => 0, (x,y) => 1 + x.max(y))

  def mapViaFold[B](f: A => B): Tree[B] = this.fold(i => Leaf(f(i)), (x,y) => Branch(x,y))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l,r) =>
      val leftPositive = l.firstPositive
      if leftPositive > 0 then leftPositive else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l,r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold((x) => x, (x, y) => x.max(y))

  @main
  def main(): Unit =
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    println(t.depth)
    println(t.depthViaFold)
    println(t.map((i) => i + 1))
    println(t.mapViaFold((i) => i + 1))
    println(t.maximum)
    println(t.maximumViaFold)
