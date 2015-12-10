package f

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List{

  //exercise 3-22
  def addEach(ints1:List[Int], ints2:List[Int]):List[Int] = {
    (ints1, ints2) match{
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,addEach(t1,t2))
      case (Nil,Nil) => Nil
      case _ => throw new UnsupportedOperationException("different size")
    }
  }

  //exercise 3-16
  def addOne(ints:List[Int]) = foldRight(ints,Nil:List[Int])((i,tail) => Cons(i+1,tail))

  //exercise 3-14
  def append[A](as1:List[A],as2:List[A]) = foldRight(as1,as2)(Cons(_,_))

  def apply[A](as:A*):List[A] = 
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  //excercise3-4
  //TODO: Nill 渡してn>0ならエラーがでそう
  def drop[A](l:List[A], n:Int):List[A] =
    if(n==0) l else drop(tail(l),n-1)

  //excercise3-5
  def dropWhile[A](l:List[A], f:A => Boolean):List[A] =
    l match{
      case Cons(h,t) if f(h)==false => t
      case Cons(h,t) => dropWhile(t,f)
    }

  //excercise 3-15
  def flatten[A](ass:List[List[A]]) = foldRight(ass, Nil:List[A])(append(_,_))

  //exercise 3-20
  def flatMap[A,B](as: List[A])(f: A => List[B]) = flatten(map(as)(f))

  //exercise3-10
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A) => B):B =
    as match{
        case Nil => z
        case Cons(h,t) => foldLeft(t,f(z,h))(f)
      }

  def foldRight[A,B](as:List[A], z: B)(f:(A,B) =>B):B =
    as match{
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }

  //exercise 3-19
  def filter[A](as:List[A])(f:A => Boolean) = foldRight(as,Nil:List[A])((a,tail) => if(f(a)) Cons(a,tail) else tail)

  //exercise 3-21
  def filter_flatmap[A](as:List[A])(f:A => Boolean) = flatMap(as)(a => if(f(a)) List(a) else Nil)

  //exercise 3-24
  def hasSubsequence[A](sup: List[A], sub: List[A]):Boolean = 
    (sup,sub) match {
      case (Cons(a1,t1),Cons(a2,t2)) if(a1 == a2) => hasSubsequence(t1,t2)
      case (Cons(a1,t1),Cons(a2,t2)) => hasSubsequence(t1,sub)
      case (_,Nil) => true
      case _ => false
    }

  //exercise3-6
  //最後尾の要素を除去したリストを返す
  def init[A](l:List[A]):List[A] = {
    def go(l:List[A]):List[A] =
      l match{
        case Nil => Nil
        case Cons(head,Nil) => Nil
        case Cons(head,tail) => Cons(head, go(tail))
      }
    go(l)
  }

  //exercise3-9
  def length[A](as:List[A]):Int = foldRight(as,0)((_,n) => n+1)

  //exercise 3-11
  def length_foldLeft[A](as:List[A]):Int = foldLeft(as,0)((n,_) => n+1)

  //exercise 3-18
  def map[A,B](as:List[A])(f:A => B) = foldRight(as,Nil:List[B])((a,tail) => Cons(f(a),tail))

  def product(ds:List[Double]):Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x*product(xs)
  }

  //exercise 3-11
  def product_foldLeft(ints:List[Int]) = foldLeft(ints,1)(_*_)

  //exercise 3-13
  //継続渡しってやつやで
  def reverse[A](as:List[A]) = foldLeft(as,(c:List[A]) => c)((f,a) => (x:List[A]) => Cons(a,f(x)))(Nil)

  //exercise 3-3
  def setHead[A](as:List[A])(a:A) =
    Cons(a, tail(as))

  //exercise 3-17
  def string(ds:List[Double]) = foldRight(ds, Nil:List[String])((d,tail) => Cons(d.toString, tail))

  def sum(ints: List[Int]):Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  //exercise 3-11
  def sum_foldLeft(ints: List[Int]) = foldLeft(ints, 0)(_ + _)
  
  //excercise 3-2
  def tail[A](as:List[A]) = {
    as match{
      case Nil => Nil
      case Cons(h,t) => t
    }
  }

  //excercise 3-23
  def zipWith[A,B](as1:List[A], as2:List[A])(f:(A,A) => B):List[B] = 
    (as1,as2) match{
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
      case (Nil,Nil) => Nil
      case _ => throw new UnsupportedOperationException("different size")
    }

}
