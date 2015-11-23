object MyModule{
  def abs(n:Int) :Int = 
    if (n<0) -n
    else n


  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def curry[A,B,C](f: (A,B) => C):A => (B => C) = a => b => f(a,b)

  //excercise3-4
  def drop[A](l:List[A], n:Int):List[A]

  //excercise3-5
  def dropWhile[A](l:List, f:A => Boolean):List[A]

  def exercise3_1 = {
    val x = List(1,2,3,4,5) match{
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t) => h + sum(h)
      case _ => 101
    }
  }

  //末尾再帰で書いたフィボナッチ
  def fibonacci(x:Int) = {
    def go(n:Int, a:Int, b:Int) :Int = 
      if(n<2) a
      else go(n-1,b,a+b)
    go(x,0,1)
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  //exercise3.6
  //最後尾の要素を除去したリストを返す
  def init[A](l:List[A]):List[A]

  def isSorted[A](as:Array[A], orderd:(A,A) => Boolean) :Boolean = {
    //size, tail はArrayのメソッドでないので暗黙の型変換(implicit convert)が発生する
    def go(array:Array[A]):Boolean = {
      if(array.size < 2) true//lengthを呼べば暗黙型変換は発生しない
      else if(orderd(array(0),array(1))==false) false
      else go(array.tail)//tailが呼ばれる度に配列のコピーが生成される
    }
    go(as)
  }

  //excercise 3-3
  def setHead[A](as:List[A])(a:A) = {
    val head :: tail = as
    a :: tail
  }
  
  //excercise 3-2
  def tail[A](as:List[A]) = {
    val head :: tail = as
    tail
  }

  def uncurry[A,B,C](f:A => B => C): (A,B) => C = (a,b) => f(a)(b)


  def main(args: Array[String]): Unit = {
    excercise3_1
  }
}
