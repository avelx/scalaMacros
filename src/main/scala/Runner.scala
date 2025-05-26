object Runner extends App {
  import Macros._

  // Prevent compilation if conditons not met
  private val macroValue = secondMacro(11, "data")
  println(macroValue)
}
