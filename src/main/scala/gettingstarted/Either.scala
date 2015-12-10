package f

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A]{

  //exercise 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
    case Right(a) => f(a):Either[EE,B]
    case Left(e) => Left(e):Left[EE]
  }

  //exercise 4.6
  def map[B](f:A => B) = this match{
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  //exercise 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this,b) match{
    case (Right(a),Right(b)) => Right(f(a,b))
    case (Left(e),_) => Left(e)
    case (_,Left(e)) => Left(e)
  }

  //exercise 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE,B]):Either[EE, B] = this match{
    case Left(e) => b
    case Right(a) => this
  }

}

object Either{

  //exercise 4.7
  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] =
    List.foldRight(es,Right(Nil):Either[E,List[A]])( (_,_) match{
      case (Right(a),Right(tail)) => Right(Cons(a,tail))
      case (_,Left(e)) => Left(e)
      case (Left(e),_) => Left(e)
    })

  //exercise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E,List[B]] =
    List.foldRight(as,Right(Nil):Either[E,List[B]])( (e,tail) => (f(e), tail) match{
      case (Right(b),Right(tail)) => Right(Cons(b,tail))
      case (_,Left(e)) => Left(e)
      case (Left(e),_) => Left(e)
    })

}
