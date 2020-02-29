package com.prosapient.epress

import java.util.regex.{Matcher, Pattern}

import scala.collection.mutable
import scala.util.Try

/**
  * Created by Marc-André Lamothe on 30/11/16.
  */
trait Expression {
  def evaluate(data: RenderingData): Either[Double,String]
}
object Expression {

  private val decimalFormat = new java.text.DecimalFormat("#.###############")

  private val operatorPattern = Pattern.compile("""(?:'[^']*'|"[^"]*")?\s*([()%*/+-]|==|!=|<=|>=|=|<|>)\s*""")

  private val variablePattern = Pattern.compile("""^[a-zA-Z0-9_#-]+$""")

  private def generateArtifacts(oper: Matcher, source: String): Artifact = {
    var index = Try(oper.end).getOrElse(0)
    val result = mutable.Buffer[Artifact]()
    while (oper.find) {
      if (oper.start(1) > index) {
        val value = source.substring(index, oper.start(1)).trim
        if (value.nonEmpty)
          result += Value(value)
      }
      oper.group(1) match {
        case ")" =>
          return if (result.size == 1) result.head else Sequence(result)
        case "(" =>
          result += generateArtifacts(oper, source)
        case op =>
          result += Operator(op)
      }
      index = Try(oper.end).getOrElse(source.length)
    }
    if (index < source.length) {
      val value = source.substring(index).trim
      if (value.nonEmpty)
        result += Value(value)
    }
    if (result.size == 1) result.head else Sequence(result)
  }

  def parse(value: String): Expression = {
    try {
      processArtifact(generateArtifacts(operatorPattern.matcher(value), value))
    }
    catch {
      case e: ArithmeticException =>
        throw new ArithmeticException(s"Failed to parse malformed expression: $value\n${e.getMessage}")
    }
  }

  private def processArtifacts(artifacts: Seq[Artifact]): Expression = {
    if (artifacts.nonEmpty) {
      if (!artifacts.head.isInstanceOf[Operator]) {
        // Find first operator with the highest precedence
        val oper = artifacts.fold(null)({
          case (result: Operator, op: Operator) if op.priority > result.priority => op
          case (null, op: Operator) => op
          case (result, _) => result
        }).asInstanceOf[Operator]

        if (oper != null) {
          // Get the left & right hand side
          val index = artifacts.indexOf(oper)
          val left = processArtifacts(artifacts.take(index))
          val right = processArtifacts(artifacts.drop(index + 1))

          // Generate expression
          if (oper.isComparison)
            Comparison(oper.symbol, left, right)
          else {
            val result = Operation(oper.symbol.head, left, right)

            // Attempt to simplify operation with static members (without variables)
            Try(result.evaluate(null) match {
              case Left(value: Double) =>
                Number(value)
              case Right(value: String) =>
                Text(value)
            }).getOrElse(result)
          }
        }
        else
          processArtifact(artifacts.head)
      }
      else
        throw new ArithmeticException(s"Unexpected operation: ${artifacts.head.asInstanceOf[Operator].symbol}.")
    }
    else
      throw new ArithmeticException("Unexpected end of string.")
  }

  private def processArtifact(art: Artifact): Expression = {
    art match {
      case Value(value: String) =>
        if ((value.head == '\'' && value.last == '\'') || (value.head == '"' && value.last == '"'))
          Text(value.substring(1, value.length-1))
        else
          Try(Number(value.toDouble)).getOrElse({
            if (variablePattern.matcher(value).matches)
              Variable(value)
            else
              throw new ArithmeticException(s"Invalid expression: $value")
          })
      case Sequence(artifacts: Seq[Artifact]) =>
        processArtifacts(artifacts)
      case Operator(symbol: String) =>
        throw new ArithmeticException(s"Unexpected operation: $symbol.")
    }
  }

  private trait Artifact {}

  private case class Comparison(comparator: String, left: Expression, right: Expression) extends Expression {
    override def evaluate(data: RenderingData): Either[Double,String] = {
      val outcome = (left.evaluate(data), right.evaluate(data)) match {
        case (Left(a: Double), Left(b: Double)) =>   a.compareTo(b)
        case (Right(a: String), Left(b: Double)) =>  a.compareTo(decimalFormat.format(b))
        case (Left(a: Double), Right(b: String)) =>  decimalFormat.format(a).compareTo(b)
        case (Right(a: String), Right(b: String)) => a.compareTo(b)
      }
      val result = comparator match {
        case "!=" =>  outcome != 0
        case "<" =>   outcome < 0
        case "<=" =>  outcome <= 0
        case ">" =>   outcome > 0
        case ">=" =>  outcome >= 0
        case _ =>     outcome == 0
      }
      Left(if (result) 1 else 0)
    }

    override def toString: String = s"$left $comparator $right"
  }

  private case class Number(value: Double) extends Expression {
    override def evaluate(data: RenderingData): Either[Double,String] = {
      Left(value)
    }

    override def toString: String = decimalFormat.format(value)
  }

  private case class Operation(operator: Char, left: Expression, right: Expression) extends Expression {
    override def evaluate(data: RenderingData): Either[Double,String] = {
      operator match {
        case '+' =>
          (left.evaluate(data), right.evaluate(data)) match {
            case (Left(a: Double), Left(b: Double)) =>
              Left(a + b)
            case (Right(a: String), Left(b: Double)) =>
              Right(a + decimalFormat.format(b))
            case (Left(a: Double), Right(b: String)) =>
              Right(decimalFormat.format(a) + b)
            case (Right(a: String), Right(b: String)) =>
              Right(a + b)
          }
        case '-' =>
          (left.evaluate(data), right.evaluate(data)) match {
            case (Left(a: Double), Left(b: Double)) =>
              Left(a - b)
            case _ =>
              throw new ArithmeticException(s"Unsupported operation on string: -")
          }
        case '/' =>
          (left.evaluate(data), right.evaluate(data)) match {
            case (Left(a: Double), Left(b: Double)) =>
              Left(a / b)
            case _ =>
              throw new ArithmeticException(s"Unsupported operation on string: /")
          }
        case '*' =>
          (left.evaluate(data), right.evaluate(data)) match {
            case (Left(a: Double), Left(b: Double)) =>
              Left(a * b)
            case _ =>
              throw new ArithmeticException(s"Unsupported operation on string: *")
          }
        case '%' =>
          (left.evaluate(data), right.evaluate(data)) match {
            case (Left(a: Double), Left(b: Double)) =>
              Left(a % b)
            case _ =>
              throw new ArithmeticException(s"Unsupported operation on string: %")
          }
      }
    }

    override def toString: String = {
      (left match {
        case Operation('+' | '-', _, _) if operator == '%' || operator == '*' || operator == '/' =>
          s"($left)"
        case _ =>
          left.toString
      }) + s" $operator " +
      (right match {
        case Operation('+' | '-', _, _) if operator == '%' || operator == '*' || operator == '/' =>
          s"($right)"
        case _ =>
          right.toString
      })
    }
  }

  private case class Operator(symbol: String) extends Artifact {
    def isComparison: Boolean = {
      symbol match {
        case "=" | "==" | "!=" | "<" | "<=" | ">" | ">=" =>
          true
        case _ =>
          false
      }
    }

    def priority: Int = {
      symbol match {
        case "=" | "==" | "!=" | "<" | "<=" | ">" | ">=" =>
          3
        case "+" | "-" =>
          2
        case _ => // "%" | "*" | "/"
          1
      }
    }
  }

  private case class Sequence(children: Seq[Artifact]) extends Artifact

  private case class Text(text: String) extends Expression {
    override def evaluate(data: RenderingData): Either[Double,String] = {
      Right(text)
    }

    override def toString: String = "\"" + text + "\""
  }

  private case class Value(value: String) extends Artifact

  private case class Variable(name: String) extends Expression {
    override def evaluate(data: RenderingData): Either[Double,String] = {
      if (name.equalsIgnoreCase("index"))
        Left(data.iterationIndex)
      else if (data.variables.contains(name))
        data.variables(name)
      else if (data.parameters.contains(name)) {
        val value = data.parameters(name)
        Try(Left(value.toDouble)).getOrElse(Right(value))
      }
      else
        throw new NoSuchElementException(s"Undefined variable: $name")
    }

    override def toString: String = name
  }

}
