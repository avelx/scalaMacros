object DataManager {

  import quoted._

  inline def readFile(filePath: String): String  =
    ${ readFileImpl('filePath) }

   def readFileImpl(filePath: Expr[String])(using Quotes): Expr[String] = {
     val fileName = filePath.valueOrAbort
     val source = scala.io.Source.fromFile(fileName)
     val lines = { try source.mkString finally source.close() }.split("\n")

     // File data validation here
//     if lines.length != 2 then
//       throw new Error(s"File $fileName has wrong format")
     println(s"File data =>: $lines")

     Expr(s"$lines")
  }

}
