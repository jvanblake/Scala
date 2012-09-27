package funsets

object Main extends App {
  import FunSets._
  
  def func(num:Int):Boolean = 
    if(num>9)true
    else false
    
  def alter(num:Int):Int=num*num+30
  
  println(contains(singletonSet(1), 1))
  println(
      printSet(
    		  map2(
              union(
                  union(singletonSet(10),singletonSet(7)),union(singletonSet(6),singletonSet(5))),alter)))
}
