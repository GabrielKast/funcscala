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
}
