package f

sealed trait Option[+A]{

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def getOrElse[B >: A](default: => B): B =
    this match{
      case Some(value) => value
      case _ => default
    }

  def map[B](f: A => B): Option[B] =
    this match{
      case Some(value) => Some(f(value))
      case _ => None
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{

  //exercise 4.3
  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B) => C):Option[C] = (a,b) match{
    case (None,_) => None
    case (_,None) => None
    case (Some(a),Some(b)) => Some(f(a,b))
  }

  //exercise 4.4
  def sequence[A](as:List[Option[A]]):Option[List[A]] =
    List.foldRight(as,Some(Nil):Option[List[A]])((_,_) match{
      case (Some(a),Some(tail)) => Some(Cons(a,tail))
      case _ => None
    })

  //exercise 4.5
  def traverse[A,B](a:List[A])(f:A => Option[B]):Option[List[B]] =
    List.foldRight(a,Some(Nil):Option[List[B]])((a,tail) => (f(a),tail) match{
      case (Some(b),Some(tail)) => Some(Cons(b,tail))
      case _ => None
    })

}
