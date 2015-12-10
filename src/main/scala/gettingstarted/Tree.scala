package f
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  //exercise 3-27
  def depth[A](t:Tree[A]):Int =
    t match{
      case Branch(left,right) => 1 + depth(left).max(depth(right))
      case Leaf(value) => 1
    }

  //exercise 3-27
  def depth_fold[A](t:Tree[A]):Int = fold(t,(_:A)=>1)((l,r) => l.max(r)+1)

  //exercise 3-29
  def fold[A,B](t:Tree[A],z:A=>B)(f:(B,B)=>B):B =
    t match{
      case Branch(left,right) => f(fold(left,z)(f),fold(right,z)(f))
      case Leaf(value) => z(value)
    }

  //exercise 3-28
  def map[A,B](t:Tree[A])(f:A => B):Tree[B] =
    t match{
      case Branch(left,right) => Branch(map(left)(f),map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }

  //exercise 3-29
  def map_fold[A,B](t:Tree[A])(f:A => B) = fold(t,(a:A) => Leaf(f(a)):Tree[B])((left,right) => Branch(left,right))

  //exercise 3-26
  def maximum(t:Tree[Int]):Int = 
    t match{
      case Branch(left,right) => maximum(left).max(maximum(right))
      case Leaf(value) => value
    }

  //exercise 3-25
  def size[A](t:Tree[A]):Int = 
    t match{
      case Branch(left,right) => 1 + size(left) + size(right)
      case Leaf(value) => 1
    }
}
