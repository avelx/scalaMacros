object Runner extends App {
  import Macros._

  val macroValue = myMacro(4, "data")
  println(macroValue)
}
