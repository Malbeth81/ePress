package com.prosapient.inpress

import java.io.File
import java.util.regex.{Matcher, Pattern}

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
object TemplateFactory {
  private val conditionPattern = Pattern.compile("""^(?:else\s*)?if\b((?:.|\s)*?)(?:\bthen\b((?:.|\s)*?)(?:\belse((?:\b|if)(?:.|\s)*?))?)?$""")

  private val iterationPattern = Pattern.compile("""^for\b((?:.|\s)*?)(?:\bdo\b((?:.|\s)*?))?$""")

  private val tagPattern = Pattern.compile("""\$(?:([a-zA-Z0-9_#-]+)|\{((?:[^}'"]|'[^']*'|"[^"]*")*)\})""")

  private val variablePattern = Pattern.compile("""^var\s+([a-zA-Z0-9_#-]+)\s*=\s*((?:[^}'"]|'[^']*'|"[^"]*")+)$""")

  private val wordPattern = Pattern.compile("""\S+""")

  private def getFirstWord(value: String): String = {
    val matcher = wordPattern.matcher(value)
    if (matcher.find)
      matcher.group
    else
      ""
  }

  def loadFromFile(file: File): Option[Template] = {
    if (file != null && file.length > 0 && file.length < 100000000) { // 100 MB
      val sourceFile = Source.fromFile(file)
      val source = try {
        sourceFile.mkString
      }
      finally {
        sourceFile.close()
      }
      Some(TemplateImpl(tokenize(tagPattern.matcher(source), source, true)))
    }
    else
      None
  }

  private def tokenize(tag: Matcher, source: String, root: Boolean = false): Token = {
    var index = Try(tag.end).getOrElse(0)
    val result = mutable.Buffer[Token]()
    while (tag.find) {
      if (tag.start > index)
        result += StaticToken(source.substring(index, tag.start))
      if (tag.group(1) != null || tag.group(2) != null) {
        val expr = if (tag.group(1) != null) tag.group(1).trim else tag.group(2).trim
        if (expr.nonEmpty) {
          getFirstWord(expr) match {
            case "end" =>
              if (!root)
                return if (result.size == 1) result.head else GroupToken(result)
            case "else" =>
              if (!root)
                return if (result.size == 1) result.head else GroupToken(result)
            case "if" =>
              result += tokenizeCondition(expr, tag, source)
            case "for" =>
              result += tokenizeIteration(expr, tag, source)
            case "var" =>
              result += tokenizeVariable(expr)
            case _ =>
              result += ExpressionToken(Expression.parse(expr))
          }
        }
      }
      index = Try(tag.end).getOrElse(source.length)
    }
    if (index < source.length)
      result += StaticToken(source.substring(index))
    if (result.size == 1) result.head else GroupToken(result)
  }

  private def tokenizeCondition(value: String, tag: Matcher, source: String): Token = {
    val cond = conditionPattern.matcher(value)
    if (cond.matches) {
      val expr = cond.group(1).trim
      if (cond.group(2) != null && cond.group(2).trim.nonEmpty) {
        val affirmative = ExpressionToken(Expression.parse(cond.group(2).trim))
        // Inline condition
        if (cond.group(3) != null && cond.group(3).trim.nonEmpty) {
          val negative = cond.group(3).trim
          if (getFirstWord(negative) == "if")
            ConditionToken(Expression.parse(expr), affirmative, tokenizeCondition(negative, tag, source))
          else
            ConditionToken(Expression.parse(expr), affirmative, ExpressionToken(Expression.parse(negative)))
        }
        else
          ConditionToken(Expression.parse(expr), affirmative, EmptyToken)
      }
      else {
        // Block condition
        val tmp = tokenize(tag, source)
        val expr2 = if (tag.hitEnd) "end" else if (tag.group(1) != null) tag.group(1).trim else tag.group(2).trim
        if (expr2 == "end")
          ConditionToken(Expression.parse(expr), tmp, EmptyToken)
        else if (expr2.length > 4) {
          val negative = expr2.substring(4).trim
          if (getFirstWord(negative) == "if")
            ConditionToken(Expression.parse(expr), tmp, tokenizeCondition(negative, tag, source))
          else
            ConditionToken(Expression.parse(expr), tmp, StaticToken(negative))
        }
        else
          ConditionToken(Expression.parse(expr), tmp, tokenize(tag, source))
      }
    }
    else
      EmptyToken
  }

  private def tokenizeIteration(value: String, tag: Matcher, source: String): Token = {
    val iter = iterationPattern.matcher(value)
    if (iter.matches) {
      // Parse the iteration bounds
      val b = iter.group(1).trim.split("""\bto\b""")
      val (start, end) = if (b.length > 1)
        (b(0), b(1))
      else
        ("1", b(0))

      if (iter.group(2) != null && iter.group(2).trim.nonEmpty) {
        // Inline iteration
        val stmt = iter.group(2).trim
        if (getFirstWord(stmt) == "if")
          IterationToken(Expression.parse(start), Expression.parse(end), tokenizeCondition(stmt, tag, source))
        else
          IterationToken(Expression.parse(start), Expression.parse(end), ExpressionToken(Expression.parse(stmt)))
      }
      else {
        // Block iteration
        IterationToken(Expression.parse(start), Expression.parse(end), tokenize(tag, source))
      }
    }
    else
      EmptyToken
  }

  private def tokenizeVariable(value: String): Token = {
    val variable = variablePattern.matcher(value)
    if (variable.matches && variable.group(1) != null && variable.group(1).trim.nonEmpty && variable.group(2) != null && variable.group(2).trim.nonEmpty)
      VariableToken(variable.group(1).trim, Expression.parse(variable.group(2).trim))
    else
      EmptyToken
  }

  private case class ConditionToken(expression: Expression, affirmative: Token, negative: Token) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      val result = expression.evaluate(data) match {
        case Left(value: Double) =>   value > 0
        case Right(value: String) =>  value.length > 0 && value != "false"
      }
      if (result)
        affirmative.writeTo(output, data)
      else
        negative.writeTo(output, data)
    }
  }

  private object EmptyToken extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {}
  }

  private case class ExpressionToken(expression: Expression) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      val result = expression.evaluate(data) match {
        case Left(value: Double) =>   value.toString
        case Right(value: String) =>  value
      }
      output.append(result)
    }
  }

  private case class GroupToken(tokens: Seq[Token]) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      tokens.foreach(_.writeTo(output, data))
    }
  }

  private case class IterationToken(start: Expression, end: Expression, content: Token) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      (start.evaluate(data), end.evaluate(data)) match {
        case (Left(first: Double), Left(last: Double)) =>
          for (i <- first.toLong to last.toLong) {
            data.iterationIndex = i
            content.writeTo(output, data)
          }
        case (Right(x: String), _) =>
          throw new ArithmeticException(s"Invalid expression: $x")
        case (_, Right(x: String)) =>
          throw new ArithmeticException(s"Invalid expression: $x")
      }
    }
  }

  private case class StaticToken(content: String) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      output.append(content)
    }
  }

  private case class TemplateImpl(rootToken: Token) extends Template {
    private def apply(data: Map[String, String]): StringBuilder = {
      val result = StringBuilder.newBuilder
      rootToken.writeTo(result, RenderingData(0L, data, mutable.Map[String, Either[Double, String]]()))
      result
    }

    override def toArray(data: Map[String, String]): Array[Char] = {
      apply(data).toArray
    }

    override def toString(data: Map[String, String]): String = {
      apply(data).toString
    }
  }

  private trait Token {
    def writeTo(output: StringBuilder, data: RenderingData): Unit
  }

  private case class VariableToken(name: String, value: Expression) extends Token {
    override def writeTo(output: StringBuilder, data: RenderingData): Unit = {
      data.variables += (name -> value.evaluate(data))
    }
  }

}
