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

  // Ex 9 :  Compute the length of a list using foldRight.
  def length[A](l: List[A]): Int = List.foldRight(l, 0)((_, x) => x+1)

  // Ex. 10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, b) => foldLeft(b, f(z, a))(f)
  }
  
  // ex.11 implement sum, product, llength with foldLeft
  def sum(l:List[Int])=foldLeft(l, 0)((z, x)=>z+x)
  def product(l:List[Double])=foldLeft(l, 1.0)((z, x)=>z*x)
  def llength[A](l:List[A])=foldLeft(l, 0)((z, _)=>z+1)

  // EXERCISE 12: Write a function that returns the reverse of a list
  def reverse[A](l:List[A])=foldLeft(l, Nil: List[A])((z, a)=> Cons(a, z))

  // EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight?
  // How about the other way around?
  //def foldLeft2

  // EXERCISE 14: Implement append in terms of either foldLeft or foldRight.
  def appendWithFoldRight[A] (l1:List[A], l2: List[A]): List[A]= List.foldRight(l1, l2)((a, z)=> Cons(a, z))
  def appendWithFoldLeft[A] (l1:List[A], l2: List[A]): List[A]= foldLeft(reverse(l1), l2)((z, a)=> Cons(a, z))
  
  //EXERCISE 15 (hard): Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
  def concat[A](ls:List[List[A]]): List[A]=ls match {
    case Nil => Nil
    case Cons(l, tail) => appendWithFoldRight(l, concat(tail))
  }

  // EXERCISE 16: Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a pure function that returns a new List!)
  def add_one(l: List[Int]): List[Int]= l match {
    case Nil => Nil
    case Cons(a, tail) => Cons(a+1, add_one(tail))
  }
  // EXERCISE 17: Write a function that turns each value in a List[Double] into a String.
  def doublesToStrings(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(d, tail) => 
      val ds = d.toDouble.toString
      Cons(ds, doublesToStrings(tail))
  }

  // Exercice 18 :Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list. Here is its signature:15 Footnote 15mIn the standard library, map and flatMap are methods of List.
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(a, tail) => Cons(f(a), map(tail)(f))
  }

  // EXERCISE 19: Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remote all odd numbers from a List[Int].
  def filter[A](l: List[A])(f: A=>Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, tail) if f(a) => Cons(a, filter(tail)(f))
    case Cons(_, tail) => filter(tail)(f)
  }

  // EXERCISE 20: Write a function flatMap, that works like map except that the function given will return a list instead of a single result, and that list should be inserted into the final resulting list. Here is its signature:
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(a, tail) => appendWithFoldRight(f(a), flatMap(tail)(f))
  }

  // EXERCISE 21: Can you use flatMap to implement filter?
  def filterWithFlatMap[A](l: List[A])(f: A=>Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  // EXERCISE 22: Write a function that 
  // accepts two lists 
  // and constructs a new list by adding corresponding elements. 
  // For example, List(1,2,3) and List(4,5,6) becomes List(5,7,9).
  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int]=(l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(a2, tail2)) => Cons(a2, addTwoLists(Nil, tail2))
    case (Cons(a1, tail1), Nil) => Cons(a1, addTwoLists(tail1, Nil))
    case (Cons(a1, tail1), Cons(a2, tail2))=> Cons(a1+a2, addTwoLists(tail1, tail2))
  }
  
  // EXERCISE 23: Generalize the function you just wrote so that it's not specific to integers or addition.
  def mergeTwoLists[A, B, C] (as: List[A], bs: List[B])(f: (A, B)=>C): List[C]=(as, bs) match {
    case (Nil, Nil) => Nil
    case (Cons(a, taila), Cons(b, tailb)) =>
      Cons(f(a, b), mergeTwoLists(taila, tailb)(f))
    case _ => throw new UnsupportedOperationException
  }

  // EXERCISE 24 (hard): As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others. You may have some difficulty finding a concise purely functional implementation that is also efficient. That's okay. Implement the function however comes most naturally. We will return to this implementation in a couple of chapters and hopefully improve on it. Note: any two values, x, and y, can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def subsequence(l1: List[A], sub1: List[A]): Boolean = 
      (l1, sub1) match {
	case (_, Nil) => true
	case (Nil, _) => false
	case (Cons(a1, tail1), Cons(a2, tail2)) if a1==a2 => 
	  subsequence(tail1, tail2)
	case (Cons(a1, tail1), _) => subsequence(tail1, sub)
      }
    subsequence(l, sub)
  }


  //================================================================
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // EXERCISE 25: Write a function size that counts the number of nodes in a tree.
  def size[A](t:Tree[A]): Int= t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1+size(left)+size(right)
  }

  // EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int]. (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
  def maximum(t: Tree[Int]): Int= t match {
      case Leaf(v) => v
      case Branch(left, right) => 
	val maxLeft = maximum(left)
	val maxRight = maximum(right)
	if (maxLeft>maxRight) maxLeft else maxRight
  }
  // EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth[A](t:Tree[A]): Int= t match {
    case Leaf(_)=>1
    case Branch(left, right)=>
	val depthLeft = 1+depth(left)
	val depthRight = 1+depth(right)
	if (depthLeft>depthRight) depthLeft else depthRight
  }

  // EXERCISE 28: Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
  def treeMap[A, B](t:Tree[A])(f: A=>B): Tree[B]=t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(treeMap(left)(f), treeMap(right)(f))
  }

// EXERCISE 29: Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities. Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and the left and right folds for List?
  def fold[A, B](t:Tree[A])(tMap:A=>B)(tReduce:(B, B)=>B): B= t match {
    case Leaf(v)=>tMap(v)
    case Branch(left, right)=>
      tReduce(fold(left)(tMap)(tReduce), fold(right)(tMap)(tReduce))
  }

  def sizeWithFold [A](t: Tree[A]): Int=
    fold(t)(_ => 1 )(1+ _ + _ ) 
  def maxWithFold(t: Tree[Int]): Int=
    fold(t)((x: Int)=>x)((x: Int, y: Int)=> if (x>y) x else y)
  def depthWithFold[A](t: Tree[A]): Int=
    fold(t)(_=> 1 )((x: Int, y: Int)=> 1+ (if (x>y) x else y))
  def mapWithFold[A, B](t: Tree[A])(f: A=>B): Tree[B]=
    fold(t)(a => Leaf(f(a)): Tree[B])((x: Tree[B], y:Tree[B]) => Branch(x, y))


}
