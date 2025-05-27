
class MyClass {

  def myMethod(args: Int): String = {
    s"Data test: $args"
  }

}


object Runner extends App {
  import Macros._

  //def getValue: Int = 1

  // Prevent compilation if conditons not met
  private val macroValue = secondMacro( 3, "data")
  println(macroValue)

  val pmOptions = pmOption(Some(42))
  println(pmOptions)

  val optRes = optimized( List(1, 4, 5, 8, 9).map(x => x * 2).map(y => y - 1) )
  println(optRes)

  val magicInstance: MyClass = new MyClass
  //magicInstance.myMethod(42)

  // Will be an error in case method name specified incorrectly
  val magicMethod = callMeDynamically(magicInstance, "myMethod", 42)
  println(magicMethod)

}
