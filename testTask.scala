import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.collection.mutable
import scala.io.Source

sealed trait VarsCompilationError

object VarsHelper {
  def apply(code: String): Either[VarsCompilationError, VarsAST] = {
    for {
      tokens <- VarsLexer(code).right
      ast <- VarsParser(tokens).right
    } yield ast
  }
}

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
          case Gray => Left(VarsInterpreterError("Found a cycle"))
          case Black => Right(Nil)
        }
      }
      case None => {
        colors += (fileName -> Gray)
        VarsHelper(Source.fromFile(fileName).toString).map(traverse(_))
      }
    }
  }

  def traverse(root: VarsAST): Unit = root match {
    case AndThenStatement(left, right) => { traverse(left); traverse(right) }
    case ExitStatement => {
      vars.foreach( { case (name, value) => println(name.chars + " = " + value.num) })
    }
    case ImportStatement(fileName) => {
      interpret(fileName.chars)
    }
    case VarDeclarationStatement(varName, value) => {
      vars += (varName -> value)
    }
  }
}

object VarsParser extends Parsers {
  override type Elem = VarsTokens.Token

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
    VarsTokens.IMPORT ~ identifier ^^ {
      case _ ~ identifier => ImportStatement(identifier)
    }
  }

  def varDeclStatement: Parser[VarsAST] = {
    identifier ~ VarsTokens.EQUALS ~ intNum ^^ {
      case identifier ~ _ ~ intNum => VarDeclarationStatement(identifier, intNum)
    }
  }

  private def identifier: Parser[VarsTokens.Identifier] = {
    accept("identifier", { case id @ VarsTokens.Identifier(name) => id })
  }

  private def intNum: Parser[VarsTokens.IntNum] = {
    accept("identifier", { case intNum @ VarsTokens.IntNum(value) => intNum })
  }

  def apply(tokens: Seq[VarsTokens.Token]): Either[VarsParserError, VarsAST] = {
    val reader = new VarsTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(VarsParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

case class VarsParserError(msg: String) extends VarsCompilationError
case class VarsInterpreterError(msg: String) extends VarsCompilationError

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

case class VarsLexerError(msg: String) extends VarsCompilationError

object VarsLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t \n]".r

  def identifier = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { res => VarsTokens.Identifier(res.toString) }

  def equals = "=" ^^ (_ => VarsTokens.EQUALS)

  def number = """(0|[1-9]\d*)""".r ^^ { res => VarsTokens.IntNum(res.toInt) } 

  def import_ = "import" ^^ (_ => VarsTokens.IMPORT)

  def tokens = {
    phrase(rep1(import_ | equals | number | identifier)) ^^ { 
      tokens => tokens
    }
  }

  def apply(code: String): Either[VarsCompilationError, List[VarsTokens.Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(VarsLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

object VarsTokens {
  abstract class Token(chars: String)

  case class Identifier(chars: String) extends Token(chars)
  
  case class IntNum(num: Int) extends Token(num.toString)

  abstract class Keyword(chars: String) extends Token(chars)
  case object EQUALS extends Keyword("=")
  case object IMPORT extends Keyword("import")
}

object TestLexer {
  def main(args: Array[String]) = {
    Interpreter.interpret("1.txt")
  }
}