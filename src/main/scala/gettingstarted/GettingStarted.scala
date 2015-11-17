object MyModule{
  def abs(n:Int) :Int = 
    if (n<0) -n
    else n


  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def curry[A,B,C](f: (A,B) => C):A => (B => C) = a => b => f(a,b)

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

  def isSorted[A](as:Array[A], orderd:(A,A) => Boolean) :Boolean = {
    //size, tail はArrayのメソッドでないので暗黙の型変換(implicit convert)が発生する
    def go(array:Array[A]):Boolean = {
      if(array.size < 2) true//lengthを呼べば暗黙型変換は発生しない
      else if(orderd(array(0),array(1))==false) false
      else go(array.tail)//tailが呼ばれる度に配列のコピーが生成される
    }
    go(as)
  }

  def uncurry[A,B,C](f:A => B => C): (A,B) => C = (a,b) => f(a)(b)


  def main(args: Array[String]): Unit = 
    println(fibonacci(2))
    
}
