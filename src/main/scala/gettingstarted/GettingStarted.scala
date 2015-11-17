object MyModule{
  def abs(n:Int) :Int = 
    if (n<0) -n
    else n


  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def curry[A,B,C](f: (A,B) => C):A => (B => C) = a => b => f(a,b)

  //$BKvHx:F5"$G=q$$$?%U%#%\%J%C%A(B
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
    //size, tail $B$O(BArray$B$N%a%=%C%I$G$J$$$N$G0EL[$N7?JQ49(B(implicit convert)$B$,H/@8$9$k(B
    def go(array:Array[A]):Boolean = {
      if(array.size < 2) true//length$B$r8F$Y$P0EL[7?JQ49$OH/@8$7$J$$(B
      else if(orderd(array(0),array(1))==false) false
      else go(array.tail)//tail$B$,8F$P$l$kEY$KG[Ns$N%3%T!<$,@8@.$5$l$k(B
    }
    go(as)
  }

  def uncurry[A,B,C](f:A => B => C): (A,B) => C = (a,b) => f(a)(b)


  def main(args: Array[String]): Unit = 
    println(fibonacci(2))
    
}
