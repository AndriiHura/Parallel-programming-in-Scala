package reductions

import reductions.CourseTree.{Leaf, Node, Tree}

object CourseTree extends App {

  // Tree .map parallel
  sealed abstract class Tree[A] { val size: Int }
  case class Leaf[A](a: Array[A]) extends Tree[A] {
    override val size: Int = a.length
  }
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    override val size: Int = l.size + r.size
  }

  def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => {
        val len = a.length
        val b = new Array[B](len)
        for (i <- 0 until len) {
          b(i) = f(a(i))
        }
        Leaf(b)
      }
      case Node(l, r) => {
        val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
        Node(lb, rb)
      }
    }
  }
}

object CourseReduce extends App {

  // Tree .reduce parallel
  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => f(reduce(l, f), reduce(r, f))
  }

  def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => {
      val (lv, rv) = parallel(reduce(l, f), reduce(r, f))
      f(lv, rv)
    }
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Node(l, r) => Node(map(l, f), map(r, f))
  }

//  def toList[A](t: Tree[A]): List[A] = reduce[List[A]](map(t, List(_)), _ ++ _)
}
