object Macros {
  // Simple algebraic data type

  sealed trait Exp
  case class Num(value: Int) extends Exp
  case class Add(left: Exp, right: Exp) extends Exp
  case class Mul(left: Exp, right: Exp) extends Exp
  case class Sub(left: Exp, right: Exp) extends Exp
  case class Div(left: Exp, right: Exp) extends Exp


  import quoted._

  // 1 - quoting 'exp => AST
  inline def myMacro(number: Int, string: String): String =
    ${ myMacroImpl('number, 'string) }

  // 2 - splicing ${AST} => value
  def myMacroImpl(number: Expr[Int], string: Expr[String])(using Quotes): Expr[String] =
    Expr("This is a string: ")

  inline def secondMacro(number: Int, string: String): String =
    ${ secondMacroImp('number, 'string) }
  
  def secondMacroImp(num: Expr[Int], strExpr: Expr[String]) (using Quotes): Expr[String] = {
    val theNumber: Int = num.valueOrAbort
    if theNumber > 10 then 
      Expr(s"This is the number: $theNumber")
    else 
      throw new Error(s"Unxpected number: $theNumber")  
  }
}
