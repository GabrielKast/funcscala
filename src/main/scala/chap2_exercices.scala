//EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. The
// first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
// the previous two. Your definition should use a local tail-recursive function.4 
object Exercices {
  def main(args:Array[String]) {
    val l=(1 to 10).map(ex1_fibo(_))
    println("fibo:" + l)
  }
  def ex1_fibo(nth: Int):Int={
    if (nth==1) 0
    else if (nth==2) 1
    else ex1_fibo(nth-2)+ex1_fibo(nth-1);
  }
}

