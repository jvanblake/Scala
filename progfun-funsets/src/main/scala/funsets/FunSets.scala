package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set ={
    def single(num:Int):Boolean=
      if(num==elem) true
      else false
    single
  }
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    def unionST(num:Int):Boolean=
      if(!contains(s,num)) contains(t,num)
      else true
      unionST
    
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    def intersectST(num:Int):Boolean=
      if(contains(s,num)) contains(t,num)    
      else false
      intersectST
    
  } 

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    def diffST(num:Int):Boolean=
      if(contains(s,num)) !contains(t,num)
      else false
      
      diffST
    
  } 

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    def filterP(num : Int):Boolean = 
      if(contains(s,num)) p(num)
      else false
    filterP
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a==bound) true
      else if (contains(s,a))p(a)&iter(a+1) 
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a==bound) false
      else if (contains(s,a))p(a)|iter(a+1) 
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
       def mapST(num:Int):Boolean={
         if(contains(s,funcInv(num,f))) true
         else false
       }  
      mapST
  }
  
  def funcInv(num:Int,f:Int=>Int):Int ={
    def inner(i:Int):Int={
      if(i>1000) 0
      else if(f(i)==num) i
      else inner(i+1)
    }
   inner(0)
  }
  
  
  
  def map2(s: Set, f: Int => Int): Set = {
       def mapST(num:Int):Set={
         if(num==bound)(i:Int)=>false
         else if(contains(s,num))union(singletonSet(f(num)),mapST(num+1))
         else mapST(num+1) 
       }  
      mapST(-bound)
  }
  
  
  
  
  
  

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
