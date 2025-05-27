object Macros {
  // Simple algebraic data type

  sealed trait Exp

  case class Num(value: Int) extends Exp

  case class Add(left: Exp, right: Exp) extends Exp

  case class Mul(left: Exp, right: Exp) extends Exp

  case class Sub(left: Exp, right: Exp) extends Exp

  case class Div(left: Exp, right: Exp) extends Exp


  import quoted._

  // A: Basic example =>

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

  // B: Enhanced patter matching
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

  // C: More complex example how to deal with the list
  inline def optimized[A](inline list: List[A]): String =
    ${ optimizedImpl[A]('list) }

  def optimizedImpl[A: Type](list: Expr[List[A]])(using Quotes): Expr[String] = {
    val desc = list match {
      case '{
            type t3 <: A // <== This is a type restriction
            ($original: List[t1]).map[t2]($f).map[`t3`]($g)
        } => s"Some result: ${f.show} and ${g.show}"
      case l
        => "simple list"
    }
    println(desc)
    Expr(desc)
  }

  // Scala compile time: reflection
  // instance.method(42)
  inline def callMeDynamically[A](instance: A, name: String, arg: Int) =
    ${ callMeDynamicallyImp('instance, 'name, 'arg ) }

  // note: a return type of calling method is important
  def callMeDynamicallyImp[A: Type](instance: Expr[A], name: Expr[String], arg: Expr[Int])(using q: Quotes) : Expr[String] = {
    import q.reflect._ // the ability to build AST
    // typed    ASTs: Expr[A]
    // untyped/?  AST = Term

    val term = instance.asTerm

    // find the method by its name
    val method = Select.unique(term, name.valueOrAbort)
    val invocation = Apply(method, List(arg.asTerm))
    val r = invocation.asExprOf[String]
    println(s"Invocation-A: ${invocation}")
    println(s"Invocation-B: ${r.toString()}")
    r
  }

}
