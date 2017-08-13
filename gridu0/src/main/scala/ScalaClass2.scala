/**
  * @author Alexey Lyanguzov.
  */

object ScalaClass2 extends App{

  def and(x: Any, y: Any): Boolean ={
    true
  }

  println(and(true, true))
  println(and(true, false))
  println(and(false, true))
  println(and(false, false))
}
