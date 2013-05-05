object Exercices {
  //EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. 
  // The first two Fibonacci numbers are 0 and 1,
  // and the next number is always the sum of
  // the previous two. 
  // Your definition should use a local tail-recursive function.4 
  def fibo(nth: Int):Int={
    def fibo_acc(n:Int, n_plus_1:Int, i: Int):Int= {
      if (i==nth) n
      else fibo_acc(n_plus_1, n + n_plus_1, i+1)
    }
    fibo_acc(0, 1, 1)
  }

  // EXERCISE 2: Implement isSorted, which checks whether an Array[A] 
  // is sorted according to a given comparison function.
  def isSorted[A](as: Array[A], sorted: (A, A)=> Boolean): Boolean={
    if (as.length<2) true
    else (1 until as.length).forall(i=>sorted(as(i-1), as(i)))
  }

  // def partial1[A,B,C](a: A, f: (A,B) => C): B => C
  // EXERCISE 3 (hard): Implement partial1 and write down a concrete usage
  // of it. There is only one possible implementation that compiles. 
  // We don't have any concrete types here, so we can only stick things
  // together using the local 'rules of the universe' established by the type
  // signature.
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B)=> f(a, b)
  }

  // EXERCISE 4 (hard): Let's look at another example, currying, 
  // which converts a function of N arguments into a function of one argument
  // that returns another function as its result.
  // Here again, there is only one implementation that typechecks.
  // def curry[A,B,C](f: (A, B) => C): A => (B => C)
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
    (a: A) => ((b: B) => f(a, b))

    
  // Exercice 5 (optional): Implement uncurry, which reverses 
  // the transformation of curry. 
  // Note that since => associates to the right, 
  // A => (B => C) can be written as A => B => C.
  // def uncurry[A,B,C](f: A => B => C): (A, B) => C
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
      (a: A, b: B) => f(a)(b)

  // EXERCISE 6: Implement the higher-order function 
  // that composes two functions.
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}

