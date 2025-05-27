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

  def secondMacroImp(num: Expr[Int], strExpr: Expr[String])(using Quotes): Expr[String] = {
    val theNumber: Int = num.valueOrAbort
    val theString: String = strExpr.valueOrAbort
    if theNumber < 10 then {
      val res = Expr(s"This is the number: ${theString.repeat(theNumber)}")
      println(s"Executed at RT: ${res.valueOrAbort}")
      res
    }
    else {
      throw new Error(s"Unxpected number: $theNumber")
    }
  }

  inline def pmOption(inline opt: Option[Int]): String = ${ pmOptionImp('opt) }

  def pmOptionImp(opt: Expr[Option[Int]])(using Quotes): Expr[String] = {
    val res = opt match {
      case '{ Some(42) } => Expr("I found the meaning of life")
      case '{ Some($x) } => Expr(s"got a variable: ${x.show}")
      case _ => Expr("Got something else")
    }
    println(res.valueOrAbort)
    res
  }

  inline def optimized[A](inline list: List[A]): String =
    ${ optimizedImpl[A]('list) }

  def optimizedImpl[A](list: Expr[List[A]])(using Quotes): Expr[String] = {
    val desc = list match {
      case '{
//            type t1
            ($original: List[t1]).map[t2]($f).map[t3]($g)
        } => s"Some result: ${f.show} and ${g.show}"
      case l
        => "simple list"
    }
    Expr(desc)
  }
}
