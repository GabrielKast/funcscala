import org.scalatest.FunSpec

class Chapter3Spec extends FunSpec {
  import fpinscala.datastructures._
  import fpinscala.chapter3._

  describe ("ex1 is an exercice about pattern matching") {
    it ("should be 1+2=3") {
      assert(ex1()===3)
    }
  }

  describe ("ex2 : implement a tail") {
    it ("should be List(2, 3)") {
      assert(tail(List(1, 2, 3))===List(2, 3))
    }
    it ("should be Nil") {
      assert(tail(List(1))===Nil)
    }
    it("Should throw an exception") {
      intercept[UnsupportedOperationException] {
	tail(Nil)
      }
    }
  }

  describe ("ex3 : implement a drop") {
    it ("should remove 3 first elements") {
      assert(drop(List(1, 2, 3, 4, 5), 3)===List(4, 5))
    }
    it ("should remove all  elements") {
      assert(drop(List(1, 2, 3, 4, 5), 5)===Nil)
    }
    it ("should throw an exception") {
      intercept[UnsupportedOperationException] {
	drop(List(1, 2, 3, 4, 5), 6)
      }
    }
  }

  describe ("ex4 : implement a dropWhile") {
    it ("should remove 3 first elements") {
      assert(dropWhile(List(1, 2, 3, 4, 5)) (_<4) ===List(4, 5))
    }
    it ("should remove all  elements") {
      assert(dropWhile(List(1, 2, 3, 4, 5))(_<10)===Nil)
    }
    it ("should keep all the elements") {
      assert(dropWhile(List(1, 2, 3, 4, 5))(_<0)===List(1, 2, 3, 4, 5))
    }
    it ("should keep the Nil ") {
      assert(dropWhile(Nil)(x => true)===Nil)
    }
  }

  describe("Exercice 5: setHead") {
    it("should replace the first element with a 99") {
      assert(setHead(List(1, 2, 3, 4), 99)===List(99, 2, 3, 4))
      assert(setHead(List(1), 99)===List(99))
    }
    it("should not work as there is no head : exception") {
      intercept[UnsupportedOperationException] {
	setHead(Nil, 99)
      }
    }
  }

  describe ("Exercice 6 : keep about but the last element of a list") {
    it("should return the 4 first of the list") {
      assert(init(List(1, 2, 3, 4, 5))===List(1, 2, 3, 4))
    }
    it("should return Nil") {
      assert(init(List(1))===Nil)
    }
    it("should throw an exception") {
      intercept[UnsupportedOperationException] {
	init(Nil)
      }
    }
  }

  describe("Exercice 9 : Compute the length of a list using foldRight.") {
    it("should give a length=0 with Nil"){
      assert(length(Nil)===0)
    }
    it("should give a length=2 with List(1)"){
      assert(length(List(1))===1)
    }
    it("should give a length=3 with List(1, 2, 1)"){
      assert(length(List(1, 2, 1))===3)
    }
  }

  describe("Ex.10&11 : foldLeft and its use") {
    it("should return 0  when List = Nil") {
      assert(llength(Nil)===0)
    }
    it("should return llength=3 when List(1, 2, 1)") {
      assert(llength(List(1, 2, 1))===length(List(1, 2, 1)))
      assert(llength(List(1, 2, 1))===3)
    }
    it("should return 4 as sum") {
      assert(sum(List(1, 2, 1))===4)
    }
    it("should return 0 as sum when List is empty") {
      assert(sum(Nil)===0)
    }

    it("should return 4 as product") {
      assert(sum(List(1, -2, 3, -4)) === -2)
    }

    it("should return -24.0 as product") {
      assert(product(List(1, 2, 3, -4)) === -24.0)
    }

    it("should return 0.0 as product") {
      assert(product(List(1, 2, 0.0, -4)) === 0.0)
    }
    it("should return 1 as product when List is empty") {
      assert(product(Nil)===1)
    }
  }

  describe("Ex.12 : reverse") {
    it("should reverse List(1, 2, 3)") {
      assert(reverse(List(1, 2, 3))===List(3, 2, 1))
    }
    it("should reverse List(1)") {
      assert(reverse(List(1))===List(1))
    }
    it("should reverse Nil") {
      assert(reverse(Nil)===Nil)
    }
  }
}
