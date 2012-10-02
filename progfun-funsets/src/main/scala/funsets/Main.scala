package funsets

object Main extends App {
  import FunSets._
  
  def func(num:Int):Boolean = 
    (num>9)
    
  def alter(num:Int):Int=num*num+30
  
  println(contains(singletonSet(1), 1))
  println(
      printSet(
    		  map(
              union(
                  union(singletonSet(10),singletonSet(7)),union(singletonSet(6),singletonSet(5))),alter)))
}
