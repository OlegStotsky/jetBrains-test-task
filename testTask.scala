import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.collection.mutable
import scala.io.Source

sealed trait VarsCompilationError

object Interpreter {
  sealed trait Color
  case object White extends Color
  case object Gray extends Color
  case object Black extends Color

  val vars = new mutable.HashMap[VarsTokens.Identifier, VarsTokens.IntNum]()
  val colors = new mutable.HashMap[String, Color]()

  def interpret(fileName: String): Either[VarsCompilationError, Unit] = {
    colors.get(fileName) match {
      case Some(color) => {
        color match {
          case Gray => Left(VarsInterpreterError(Location(fileName, 0, 0), s"Found a cyclic import in $fileName"))
          case Black => Right(Nil)
        }
      }
      case None => {
        colors += (fileName -> Gray)
        for {
          tokens <- VarsLexer(fileName, Source.fromFile(fileName + ".vars").mkString).run.right
          ast <- VarsParser(fileName, tokens).run.right
          _ <- Right(traverse(ast))
          _ <- Right(colors.update(fileName, Black))
        } yield ()
      }
    }
  }

  def traverse(root: VarsAST): Unit = root match {
    case AndThenStatement(left, right) => { traverse(left); traverse(right) }
    case ExitStatement => ()
    case ImportStatement(fileName) => {
      interpret(fileName.chars)
    }
    case VarDeclarationStatement(varName, value) => {
      vars += (varName -> value)
    }
  }

  def apply(fileName: String) = {
    interpret(fileName)
  }
}

case class VarsParser(fileName: String, reader: VarsTokenReader) extends Parsers {
  override type Elem = VarsTokens.Token

  def run = {
    program(reader) match {
      case NoSuccess(msg, next) => Left(VarsParserError(Location(fileName, next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[VarsAST] = {
    phrase(block)
  }

  def block: Parser[VarsAST] = {
    rep1(statement) ^^ { case stmtList => AndThenStatement(stmtList.reduceRight(AndThenStatement), ExitStatement) }
  }

  def statement: Parser[VarsAST] = {
    importStatement | varDeclStatement
  }

  def importStatement: Parser[VarsAST] = {
    VarsTokens.Import() ~ identifier ^^ {
      case _ ~ identifier => ImportStatement(identifier)
    }
  }

  def varDeclStatement: Parser[VarsAST] = {
    identifier ~ VarsTokens.Equals() ~ intNum ^^ {
      case identifier ~ _ ~ intNum => VarDeclarationStatement(identifier, intNum)
    }
  }

  private def identifier: Parser[VarsTokens.Identifier] = {
    accept("identifier", { case id @ VarsTokens.Identifier(name) => id })
  }

  private def intNum: Parser[VarsTokens.IntNum] = {
    accept("identifier", { case intNum @ VarsTokens.IntNum(value) => intNum })
  }
}

case object VarsParser extends Parsers {
  def apply(fileName: String, tokens: Seq[VarsTokens.Token]) = {
    val reader = new VarsTokenReader(tokens)
    val parser = new VarsParser(fileName, reader)
    
    parser
  }
}

case class VarsParserError(location: Location, msg: String) extends VarsCompilationError
case class VarsInterpreterError(location: Location, msg: String) extends VarsCompilationError

case class Location(file: String, line: Int, column: Int) {
  override def toString = s"$line: $column"
}

sealed trait VarsAST
case class ImportStatement(fileName: VarsTokens.Identifier) extends VarsAST
case class VarDeclarationStatement(varName: VarsTokens.Identifier, value: VarsTokens.IntNum) extends VarsAST
case class AndThenStatement(step1: VarsAST, step2: VarsAST) extends VarsAST
case object ExitStatement extends VarsAST

class VarsTokenReader(tokens: Seq[VarsTokens.Token]) extends Reader[VarsTokens.Token] {
  override def first: VarsTokens.Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[VarsTokens.Token] = new VarsTokenReader(tokens.tail)
}

case class VarsLexerError(location: Location, msg: String) extends VarsCompilationError

case class VarsLexer(fileName: String, code: String) extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t \n]".r

  def run = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(VarsLexerError(Location(fileName, next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def identifier = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { res => VarsTokens.Identifier(res.toString) }
  }

  def equals = positioned { 
    "=" ^^ (_ => VarsTokens.Equals())
  }

  def number = positioned {
    """(0|[1-9]\d*)""".r ^^ { res => VarsTokens.IntNum(res.toInt) } 
  }

  def import_ = positioned {
    "import" ^^ (_ => VarsTokens.Import())
  }

  def tokens = {
    phrase(rep1(import_ | equals | number | identifier)) ^^ { 
      tokens => tokens
    }
  }
}

object VarsTokens {
  abstract class Token(chars: String) extends Positional

  case class Identifier(chars: String) extends Token(chars) 
  
  case class IntNum(num: Int) extends Token(num.toString)

  abstract class Keyword(chars: String) extends Token(chars)
  case class Equals() extends Keyword("=")
  case class Import() extends Keyword("import")
}

object TestLexer {
  def main(args: Array[String]) = {
    Interpreter.interpret(args(1)) match {
      case Left(err) => {
        err match {
          case VarsLexerError(location, msg) => println(s"Error while lexing: (${location.file}, ${location.line}, ${location.column})", msg)
          case VarsParserError(location, msg) => println("Error while parsing: ", msg)
          case VarsInterpreterError(location, msg) => println("Error while interpreting: ", msg)
        }
      }
      case Right(_) => {
        Interpreter.vars.foreach({ case (name, value) => println(name.chars + " = " + value.num)})
      }
    }

    Unit
  }
}