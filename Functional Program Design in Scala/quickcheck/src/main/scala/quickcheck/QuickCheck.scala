package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(Gen.const(empty),
    for {
      a <- arbitrary[Int]
      h <- oneOf(Gen.const(empty), genHeap)
    } yield insert(a, h)
  )
//  lazy val genHeap: Gen[H] = for {
//    a <- arbitrary[Int]
//    h <- oneOf(Gen.const(empty), genHeap)
//  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  //If you insert any two elements into an empty heap, finding the minimum of
  // the resulting heap should get the smallest of the two elements back.

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a < b) a else b)
  }

  //If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("delete1") = forAll {a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  //Given any heap, you should get a sorted sequence of elements when continually
  // finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll {(h: H) =>
    def isSorted(h:H): Boolean ={
      if (isEmpty(h)) true else {
        val min = findMin(h)
        val newHeap = deleteMin(h)
        isSorted(newHeap)||min < findMin(newHeap)
      }
    }
    isSorted(h)
  }

  //Finding a minimum of the melding of any two heaps should return
  // a minimum of one or the other.
  property("mergedMin") = forAll {(h1: H, h2: H) =>
    def checkMergedMin(h1: H, h2: H):Boolean ={
      val hMerged = meld(h1, h2)
      if (isEmpty(h1) && isEmpty(h2)) true
      else if(isEmpty(h1)) findMin(h2) == findMin(hMerged)
      else if(isEmpty(h2)) findMin(h1) == findMin(hMerged)
      else math.min(findMin(h1), findMin(h2)) == findMin(hMerged)
    }
    checkMergedMin(h1,h2)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean ={
      if (isEmpty(h1) && isEmpty(h2)) true else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    }
    def helper(h1: H, h2: H): Boolean ={
      if (isEmpty(h1)) true
      else {
        heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
      }
    }
    helper(h1, h2)
  }

}
