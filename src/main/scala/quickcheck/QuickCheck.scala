package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
 
  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(e, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("insert2") = forAll { (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == min(a,b)
  }

  property("deleteMin") = forAll { (a: A) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }
    
 property("sortedSequence") = forAll { (h: H) =>
    def helper(h: H): List[A] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: helper(deleteMin(h))
    }
    val L = helper(h)
    L == L.sorted
  }
 
 property("minMeld") = forAll { (h1: H, h2: H) =>
    val m = findMin(meld(h1,h2))
    m == findMin(h1) || m == findMin(h2) 
  }
 
 property("totalMeld") = forAll { (h1: H, h2: H) =>
   def helper(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: helper(deleteMin(h))
    }
   
    val h3 = meld(h1,h2)
    helper(h3).length == helper(h1).length + helper(h2).length 
  }
 
 property("sameElemTwice") = forAll { (a: A) =>
    val h1 = insert(a, insert(a, empty))
    val m1 = findMin(h1)
    val h2 = deleteMin(h1)
    if (!isEmpty(h2)) {
      val m2 = findMin(h2)
      val h3 = deleteMin(h2)
      m1 == m2 && isEmpty(h3) == true
    } else false
  }
 
 
 property("meldSameHeap") = forAll { (h: H) =>
   def helper(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: helper(deleteMin(h))
    }
   
    val h1 = meld(h, h)
    val m1 = findMin(h1)
    val h2 = deleteMin(h1)
    val m2 = findMin(h2)
    m1 == m2 && helper(h1).length == 2 * helper(h).length
  }
 
 
 property("p1") = forAll { (h: H) =>
   def helper(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: helper(deleteMin(h))
    }
   
    val h1 = insert(1, insert(2, insert(3, empty)))
    val h2 = insert(4, insert(5, insert(6, empty)))
    val h3 = meld(h1, h2)
    val m1 = findMin(h3)
    val h4 = deleteMin(h3)
    val m2 = findMin(h4)
    m1 == 1 && m2 == 2
   }
 
 property("p2") = forAll { (h1: H, h2: H, h3: H, h4: H, h5: H, h6: H) =>
   def helper(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: helper(deleteMin(h))
    }
   
    val q1 = meld(h1, h2)
    val m1 = findMin(q1)
    val q2 = deleteMin(q1)
    
    val q3 = meld(h3, q2)
    val m2 = findMin(q3)
    val q4 = deleteMin(q3)
    
    val q5 = meld(h4, q4)
    val m3 = findMin(q5)
    val q6 = deleteMin(q5)
    
    val q7 = meld(h5, q6)
    val m4 = findMin(q7)
    val q8 = deleteMin(q7)
    
    val q9 = meld(h6, q8)
    val m5 = findMin(q9)
    val q10 = deleteMin(q9)
    
    val a1 = (m1 == findMin(h1) || m1 == findMin(h2))
    val a2 = (m2 == findMin(h3) || m2 == findMin(q2))
    val a3 = (m3 == findMin(h4) || m3 == findMin(q4)) 
    val a4 = (m4 == findMin(h5) || m4 == findMin(q6)) 
    val a5 = (m5 == findMin(h6) || m5 == findMin(q8)) 
    a1 && a2 && a3 && a4 && a5
   }
 
 property("p3") = forAll { (h: H) =>
   def helper(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: helper(deleteMin(h))
    }
   val h1 = insert(1, insert(2, insert(6, insert(3, insert(5, insert(4, empty))))))
   val h2 = insert(7, insert(9, insert(8, empty)))
   val h3 = insert(10, empty)
   val h4 = meld(meld(h1, h2), h3)
   val L = helper(h4)
   //println(L)
   L == List(1,2,3,4,5,6,7,8,9,10)
 }
}
