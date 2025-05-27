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

}
