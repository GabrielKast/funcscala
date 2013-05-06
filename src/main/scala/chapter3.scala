/* ============================================= */

package fpinscala

object chapter3 {
  import fpinscala.datastructures._
  //EXERCISE 1: What will the result of the following match expression be?
  def ex1() = 
    List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

  // EXERCISE 2: Implement the function tail for "removing" the first element
  // of a List. Notice the function takes constant time. What are different
  // choices you could make in your implementation if the List is Nil? 
  // We will return to this question in the next chapter.
  def tail[A](l: List[A]): List[A]=l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, rest) => rest
  }

  // EXERCISE 3: Generalize tail to the function drop, 
  // which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n==0) l
    else drop(tail(l), n-1)
  }


  // EXERCISE 4: Implement dropWhile, which removes elements from the
  // List prefix as long as they match a predicate. Again, notice 
  // these functions take time proportional only to the number of elements 
  // being dropped—we do not need to make a copy of the entire List.

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, rest) if f(a) => dropWhile(rest)(f)
    case _ => l
  }

  // EXERCISE 5: Using the same idea, implement the function setHead for
  // replacing the first element of a List with a different value.
  def setHead[A](l: List[A], a: A): List[A]= l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, tail) => Cons(a, tail)
  }

  // EXERCISE 6: Implement a function, init, which returns a List consisting 
  // of all but the last element of a List. 
  // So, given List(1,2,3,4), init will return List(1,2,3).
  // Why can't this function be implemented in constant time like tail?
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(a, Nil ) => Nil
    case Cons(a, b) => Cons(a, init(b))
  }
}
