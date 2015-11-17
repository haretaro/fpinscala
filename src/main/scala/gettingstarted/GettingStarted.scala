object MyModule{
  def abs(n:Int) :Int = 
    if (n<0) -n
    else n


  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def curry[A,B,C](f: (A,B) => C):A => (B => C) = a => b => f(a,b)

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
    def go(array:Array[A]):Boolean = {
      if(array.size < 2) true
      else if(orderd(array(0),array(1))==false) false
      else go(array.tail)
    }
    go(as)
  }

  def uncurry[A,B,C](f:A => B => C): (A,B) => C = (a,b) => f(a)(b)


  def main(args: Array[String]): Unit = 
    println(fibonacci(2))
    
}
