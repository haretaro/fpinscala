package laziness

import Stream._

trait Stream[+A]{
  //exercise 5.1
  def toList: List[A] = this match{
      case Empty => Nil
      case Cons(hd,tl) => hd() :: tl().toList
  }

  //exercise 5.2
  def take(n:Int):Stream[A] = (this,n) match {
    case (Empty,_) => Empty
    case (_,n) if n <= 0 => Empty
    case (Cons(hd,tl),n) => Cons(hd,() => tl().take(n-1))
  }

  //exercise 5.2
  def drop(n:Int):Stream[A] = (this,n) match{
    case (Empty,_) => Empty
    case (s,n) if n <= 0 => s
    case (Cons(hd,tl),n) => tl().drop(n-1)
  }

  //exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(hd,tl) if p(hd()) => Cons(hd,() => tl().takeWhile(p))
    case _ => Empty
  }

  //exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match{
    case Empty => true
    case Cons(h,t) => p(h()) && t().forAll(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match{
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  //exercise 5.5
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A]){
      case (a,b) if (p(a)) => Cons(() => a,() => b)
      case (_,b) => Empty
    }

  //exercise 5.6
  def headOption =
    foldRight(None:Option[A])((a,b) => Some(a))

  //exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => cons(f(a),b))

  //exercise 5.7
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]){
      case (a,b) if(p(a)) => cons(a, b)
      case (_,b) => b
    }

  //exercise 5.7
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s){
      case (a,b) => Cons(() => a, () => b)
    }

  //exercise 5.7
  def flatMap[A, B](s: Stream[A])(f: A => Stream[B]): Stream[B] =
    s.foldRight(Empty: Stream[B])((a, acc) => f(a).append(acc))

  //exercise 5.12
  def mapUF[B](f: A => B) = unfold(this){
    case Cons(h,t) => Some((f(h()),t()))
    case _ => None
  }

  def takeUF(n: Int): Stream[A] = unfold((this,n)){
    case (_,0) => None
    case (Cons(h,t),n) => Some(h(), (t(), n-1))
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

  //exercise 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  //exercise 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  //exercise 5.10
  def fibs: Stream[Int] = {
    def fib(acc1: Int, acc2: Int): Stream[Int] =
      cons(acc1, fib(acc2, acc1+acc2))
    fib(0,1)
  }

  //exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match{
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => Empty
    }

  def fibsUF = unfold((0,1))(t => Some((t._1,(t._2, t._1 + t._2))))
}
