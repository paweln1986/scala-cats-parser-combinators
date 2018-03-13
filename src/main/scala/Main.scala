import Parsers.{ParseResult, Parser}
import cats.{Applicative, Functor, Monad}

import scala.io.Source

class ParserFunctor extends Functor[Parser] {
  override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = {
    def parse(input: String) = fa.parse(input).map { case (result, remaining) => (f(result), remaining) }

    Parser(parse)
  }
}

class ParserApplicative extends ParserFunctor with Applicative[Parser] {
  override def pure[A](x: A): Parser[A] = Parser({ input => Some((x, input)) })

  override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = {
    def parse(input: String) = for {
      (f, remaining) <- ff.parse(input)
      (result, remaining1) <- fa.parse(remaining)
    } yield (f(result), remaining1)

    Parser(parse)
  }
}

class ParserMonad extends ParserApplicative with Monad[Parser] {
  override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
    def parse(input: String) = for {
      (result, remaining) <- fa.parse(input)
      (result1, remaining1) <- f(result).parse(remaining)
    } yield (result1, remaining1)

    Parser(parse)
  }

  override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
    def go(input: String, nextResult: A): ParseResult[B] = {
      f(nextResult).parse(input) match {
        case None => None
        case Some((Left(next), remaining)) => go(remaining, next)
        case Some((Right(result), remaining)) => Some((result, remaining))
      }
    }

    new Parser[B](go(_, a))
  }
}

object Parsers {

  import cats.instances.all._
  import cats.syntax.all._

  implicit val parserApplicative: ParserMonad = new ParserMonad

  type ParseResult[A] = Option[(A, String)]

  implicit class ParserOps[A](p: Parser[A]) {
    def <|>(secondParser: => Parser[A]): Parser[A] = {
      def parse(input: String) = p.parse(input) match {
        case None => secondParser.parse(input)
        case success => success
      }

      Parser[A](parse)
    }
  }

  case class Parser[A](parse: (String => ParseResult[A]))


  def between[A, B, C](left: Parser[A], center: Parser[B], right: Parser[C]): Parser[B] = left *> center <* right

  def some[A](p: Parser[A]): Parser[List[A]] = (p, many(p)).mapN(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] = List[A]().tailRecM { elems =>
    new Parser[Either[List[A], List[A]]]({ input =>
      p.parse(input) match {
        case None => Some((Right(elems.reverse), input))
        case Some((a, remaining)) => Some(Left(a :: elems), remaining)
      }
    })
  }

  val newLine: Parser[Unit] = conditional(_ == '\n').map(_ => ())

  val whitespace: Parser[Unit] = conditional(_.isWhitespace).map(_ => ())

  val whitespaces: Parser[Unit] = many(whitespace).map(_ => ())

  val whitespaces1: Parser[Unit] = some(whitespace).map(_ => ())

  def sepBy1[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = (p, many(sep *> p)).mapN(_ :: _)

  val alphaNum1: Parser[String] = (conditional(_.isLetter), many(conditional(_.isLetterOrDigit)))
    .mapN(_ :: _)
    .map(_.mkString)

  val int: Parser[Int] = some(conditional(_.isDigit)).map(_.mkString).map(_.toInt)

  val EOF: Parser[Unit] = Parser[Unit]({input =>
    if(input.isEmpty)
      Some((),"")
    else
      throw new IllegalStateException("parser is not empty")
  })

  val float: Parser[Float] = (int, char('.'), int).mapN(_.toString + _ + _.toString).map(_.toFloat)

  def char(c: Char): Parser[Char] = conditional(_ == c)

  def string(s: String): Parser[String] = s.map(char).toList.sequence[Parser, Char].map(_.mkString)

  def conditional(p: Char => Boolean): Parser[Char] = {
    def parse(input: String) = input.headOption.filter(p).map(result => (result, input.tail))

    Parser[Char](parse)
  }

}

object ConfigFileParser {

  import Parsers._
  import cats.syntax.all._

  sealed trait ConfigValue

  final case class IntValue(value: Int) extends ConfigValue

  final case class StringValue(value: String) extends ConfigValue

  final case class FloatValue(value: Float) extends ConfigValue

  final case class SectionName(name: String) extends AnyVal

  final case class Assignment(name: String, value: ConfigValue)

  final case class Section(sectionName: SectionName, assignments: List[Assignment])

  val configValue: Parser[ConfigValue] =
    float.map[ConfigValue](FloatValue) <|>
      int.map(IntValue) <|>
      alphaNum1.map(StringValue)

  val assignment: Parser[Assignment] = (alphaNum1 <* between(whitespaces, char('='), whitespaces), configValue).mapN(Assignment)

  private val sectionNameValue: Parser[String] = whitespaces1 *> alphaNum1 <* whitespaces1

  val sectionName: Parser[SectionName] = between(string("[["), sectionNameValue, string("]]")).map(SectionName)

  val section: Parser[Section] = for {
    name <- sectionName
    _ <- newLine
    assignments <- sepBy1(assignment, newLine)
  } yield Section(name, assignments)

  val confingFile: Parser[List[Section]] = sepBy1(section, some(newLine))

}


object Main extends App {

  import ConfigFileParser._

  val a = Source.fromResource("config.txt").getLines.mkString("\n")
  println(confingFile.parse(a))

}
