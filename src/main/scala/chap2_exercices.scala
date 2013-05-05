object Exercices {
  def main(args:Array[String]) {
    val l=(1 to 10).map(ex1(_))
    println("fibo:" + l)
  }
  //EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. 
  // The first two Fibonacci numbers are 0 and 1,
  // and the next number is always the sum of
  // the previous two. 
  // Your definition should use a local tail-recursive function.4 
  def ex1(nth: Int):Int={
    def fibo_acc(n:Int, n_plus_1:Int, i: Int):Int= {
      if (i==nth) n
      else fibo_acc(n_plus_1, n + n_plus_1, i+1)
    }
    fibo_acc(0, 1, 1)
  }

  // EXERCISE 2: Implement isSorted, which checks whether an Array[A] 
  // is sorted according to a given comparison function.
  def ex2[A](as: Array[A], sorted: (A, A)=> Boolean): Boolean={
    if (as.length<2) true
    else (1 until as.length).forall(i=>sorted(as(i-1), as(i)))
  }

  // def partial1[A,B,C](a: A, f: (A,B) => C): B => C
  // EXERCISE 3 (hard): Implement partial1 and write down a concrete usage
  // of it. There is only one possible implementation that compiles. 
  // We don't have any concrete types here, so we can only stick things
  // together using the local 'rules of the universe' established by the type
  // signature.
  def ex3[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B)=> f(a, b)
  }
}

