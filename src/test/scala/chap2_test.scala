import org.scalatest.FunSpec

class Chapter2Spec extends FunSpec {

  describe("Exercice 1: tailrec version of Fibo ") {
    it("should give 0 for the first result") {
      assert(Exercices.ex1(1)===0)
    }
    it("should give 1 for the second result") {
      assert(Exercices.ex1(2)===1)
    }
    it("should give 35 for the 10nth result") {
      assert(Exercices.ex1(2)===1)
    }
  }

  describe("Exercice 2 : is_sorted?") {
    val int_sorter = (a: Int,b: Int)=> a<b
    val string_sorter = (a: String,b: String)=> a<b
    it("should be sorted with ints") {
      assert(Exercices.ex2(Array(1, 2, 3, 4), int_sorter)===true)
    }
    it("should not be sorted simple") {
      assert(Exercices.ex2(Array(1, 2, 1, 3, 4), int_sorter)===false)
    }
    it("should not be sorted - not sorted at the end of the array") {
      assert(Exercices.ex2(Array(1, 2, 3, 4, 1), int_sorter)===false)
    }
    it("should not be sorted - not sorted at the beginning of the array") {
      assert(Exercices.ex2(Array(2, 1, 2, 3, 4), int_sorter)===false)
    }
    it("should be sorted with strings") {
      assert(Exercices.ex2(Array("la", "mi", "re", "sol"), string_sorter)===true)
    }
  }
  
  describe("Exercice 3 : Partial1") {
    it ("should give a funtion that adds 1") {
      val result=Exercices.ex3(1, (a: Int, b:Int)=> a+b)
      assert(result(2)===3)
      assert(result(3)===4)
    }
    it ("should give a funtion that gives string.length * Int /10 ") {
      val f = (a: Int, b:String)=> (a * b.length)/10.0
      val result=Exercices.ex3(3, f)
      assert(result("cheval")===1.8)
      assert(result("2 lumières")===3.0)
    }
  }

}
