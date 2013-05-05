import org.scalatest.FunSpec

class Chapter2Spec extends FunSpec {

  describe("Exercice 1: tailrec version of Fibo ") {
    it("should give 0 for the first result") {
      assert(Exercices.fibo(1)===0)
    }
    it("should give 1 for the second result") {
      assert(Exercices.fibo(2)===1)
    }
    it("should give 35 for the 10nth result") {
      assert(Exercices.fibo(2)===1)
    }
    it("should give the first 10 fibo numbers") {
      val l=(1 to 10).map(Exercices.fibo(_))
      assert(l === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }
  }

  describe("Exercice 2 : is_sorted?") {
    val int_sorter = (a: Int,b: Int)=> a<b
    val string_sorter = (a: String,b: String)=> a<b
    it("should be sorted with ints") {
      assert(Exercices.isSorted(Array(1, 2, 3, 4), int_sorter)===true)
    }
    it("should not be sorted simple") {
      assert(Exercices.isSorted(Array(1, 2, 1, 3, 4), int_sorter)===false)
    }
    it("should not be sorted - not sorted at the end of the array") {
      assert(Exercices.isSorted(Array(1, 2, 3, 4, 1), int_sorter)===false)
    }
    it("should not be sorted - not sorted at the beginning of the array") {
      assert(Exercices.isSorted(Array(2, 1, 2, 3, 4), int_sorter)===false)
    }
    it("should be sorted with strings") {
      assert(Exercices.isSorted(Array("la", "mi", "re", "sol"), string_sorter)===true)
    }
  }
  
  describe("Exercice 3 : Partial1") {
    it ("should give a funtion that adds 1") {
      val result=Exercices.partial1(1, (a: Int, b:Int)=> a+b)
      assert(result(2)===3)
      assert(result(3)===4)
    }
    it ("should give a funtion that gives string.length * Int /10 ") {
      val f = (a: Int, b:String)=> (a * b.length)/10.0
      val result=Exercices.partial1(3, f)
      assert(result("cheval")===1.8)
      assert(result("2 lumières")===3.0)
    }
  }

  describe("Exercice 4 : currying") {
    it ("should send in response a function based on currying") {
      val f = (a: Int, b: Int)=> a+b
      val g = Exercices.curry(f)
      val g_1 = g(1)
      assert(f(3, 1)===g_1(3) )
    }
  }

  describe("Exercice 5 : uncurrying") {
    it ("should send in response an uncurried function") {
      val f = (a: Int) => (b: Int) => a+b
      val f_1 = f(1)
      val g = Exercices.uncurry(f)
      assert(g(3, 1)===f_1(3) )
    }
    it ("should send in response an uncurried function with a multiply") {
      val f = (a: Int) => (b: Int) => a*b
      val f_4 = f(4)
      val g = Exercices.uncurry(f)
      assert(g(3, 4)===f_4(3) )
    }
  }

}
