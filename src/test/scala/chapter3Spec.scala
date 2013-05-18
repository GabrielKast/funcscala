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

  describe ("Ex 14 : EXERCISE 14: Implement append in terms of either foldLeft or flodRight") {
    it("should concatenate two lists") {
      assert(appendWithFoldLeft(List(1, 2, 3), List(4, 5, 6))===List(1, 2, 3, 4, 5, 6))
    }
    it("should concatenate one List and Nil") {
      assert(appendWithFoldLeft(List(1, 2, 3), Nil)===List(1, 2, 3))
    }
    it("should concatenate one Nil and one list") {
      assert(appendWithFoldLeft(Nil, List(4, 5, 6))===List(4, 5, 6))
    }

    it("should concatenate two lists appendWithFoldRight") {
      assert(appendWithFoldRight(List(1, 2, 3), List(4, 5, 6))===List(1, 2, 3, 4, 5, 6))
    }
    it("should concatenate one List and Nil with appendWithFoldRight") {
      assert(appendWithFoldRight(List(1, 2, 3), Nil)===List(1, 2, 3))
    }
    it("should concatenate one Nil and one list withappendWithFoldRight") {
      assert(appendWithFoldRight(Nil, List(4, 5, 6))===List(4, 5, 6))
    }
  }

  describe("Ex.15 concat") {
    it("should concatenate two lists"){
      assert(concat(List(List(1, 2, 3), List(4, 5, 6)))===List(1, 2, 3, 4, 5, 6))
    }
    it("should concatenate three lists"){
      assert(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))===List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
    it("should concatenate 6 lists"){
      assert(concat(List(List(1), List(2, 3), List(4, 5, 6), List(7), Nil, List(8, 9)))===List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
    it("should concatenate zero lists"){
      assert(concat(Nil)===Nil)
    }
  }


  describe("EXERCISE 16: a function that transforms a list of integers by adding 1") {
    it("should add 1 to all elements"){
      assert(add_one(List(1, 2, -3))===List(2, 3, -2))
    }
    it("should not break with a Nil list"){
      assert(add_one(Nil)===Nil)
    }
  }

  describe("EXERCISE 17: Write a function that turns each value in a List[Double] into a String.") {
    it("should create a list of Strings"){
      assert(doublesToStrings(List(1.0, 2.1, -3)) ===List("1.0", "2.1", "-3.0"))
    }
    it("should not break with a Nil list"){
      assert(doublesToStrings(Nil: List[Double])===Nil)
    }
  }

  describe("EXERCISE 18:map") {
    it("should add 1 to all elements"){
      assert(map(List(1, 2, -3)) (_+1) ===List(2, 3, -2))
    }
    it("should not break with a Nil list"){
      assert(map(Nil: List[Int])(x=>x+1)===Nil)
    }
  }

  describe("EXERCISE 19:filter") {
    it("should get all odds number"){
      assert(filter(List(1, 2, -3)) (x=> x%2==0) ===List(2))
    }
    it("should get all odds number - sample nb2"){
      assert(filter(List(1, 2, 4)) (x=> x%2==0) ===List(2, 4))
    }
    it("should get all odds number - Nil result"){
      assert(filter(List(1, 3, 5)) (x=> x%2==0) ===Nil)
    }
    it("should not break with a Nil list"){
      assert(filter(Nil: List[Int])(x=>false)===Nil)
    }
  }

  describe("Ex: 20 flatMap"){
    it("should work as a good old flatMap") {
      assert(flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3))
    }
  }


  describe("EXERCISE 21:filter with a flatMap") {
    it("should get all odds number"){
      assert(filterWithFlatMap(List(1, 2, -3)) (x=> x%2==0) ===List(2))
    }
    it("should get all odds number - sample nb2"){
      assert(filterWithFlatMap(List(1, 2, 4)) (x=> x%2==0) ===List(2, 4))
    }
    it("should get all odds number - Nil result"){
      assert(filterWithFlatMap(List(1, 3, 5)) (x=> x%2==0) ===Nil)
    }
    it("should not break with a Nil list"){
      assert(filterWithFlatMap(Nil: List[Int])(x=>false)===Nil)
    }
  }

  describe("EXERCISE 22 : Construct a new list in addition of two lists") {
    it("should give Nil if both lists are Nil") {
      assert(addTwoLists(Nil: List[Int], Nil: List[Int])===Nil)
    }
    it("should add two lists") {
      assert(addTwoLists(List(1, 2, 3), List(4, 5, 6))===List(5, 7, 9))
    }
    it("should add two lists even if the first one is bigger than the other") {
      assert(addTwoLists(List(1, 2, 3, 4), List(4))===List(5, 2, 3, 4))
    }
    it("should add two lists even if the second one is bigger than the other") {
      assert(addTwoLists(List(1, 2), List(4, 5, 6))===List(5, 7, 6))
    }
  }

  describe("EXERCISE 23 : Generalize the function you just wrote so that it's not specific to integers or addition.") {
    it("should give Nil if both lists are Nil") {
      assert(mergeTwoLists(Nil: List[Int], Nil: List[Int])((x, y)=>x*y)===Nil)
    }
    it("should add two lists") {
      assert(mergeTwoLists(List(1, 2, 3), List(4.0, 5.0, 6.1))((x, y)=>x+y)===List(5.0, 7.0, 9.1))
    }
    it("should multiply two lists") {
      assert(mergeTwoLists(List(1, 2, 3), List(-1.0, -2.0, 3))((x, y)=>x*y)===List(-1.0, -4.0, 9.0))
    }
  }
}
