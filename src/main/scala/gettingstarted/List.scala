package f

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List{

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

  def product(ds:List[Double]):Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x*product(xs)
  }

  //excercise 3-3
  def setHead[A](as:List[A])(a:A) =
    Cons(a, tail(as))

  def sum(ints: List[Int]):Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  //excercise 3-2
  def tail[A](as:List[A]) = {
    as match{
      case Nil => Nil
      case Cons(h,t) => t
    }
  }

}
