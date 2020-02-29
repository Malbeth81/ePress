package com.prosapient.epress

import org.scalatest._

import scala.collection.mutable
import scala.compat.Platform

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
class ExpressionSpec extends FlatSpec with Matchers {

  behavior of "Expression"

  // Warm up
  Expression.parse("(0 + 1) * 2 < (2 - 1) / 2")

  it should "parse a single value" in {
    val e = "2342"
    val start = Platform.currentTime
    val expression = Expression.parse(s"($e)")
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual e
    assert(time < 10)
  }

  it should "parse a simple expression" in {
    val e = "(a + 2) * 3"
    val start = Platform.currentTime
    val expression = Expression.parse(e)
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual e
    assert(time < 10)
  }

  it should "parse a simple comparison" in {
    val e = "a - 2 < b + 1"
    val start = Platform.currentTime
    val expression = Expression.parse(e)
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual e
    assert(time < 10)
  }

  it should "close opened parentheses at the end of string" in {
    val e = "a / (b - 1"
    val start = Platform.currentTime
    val expression = Expression.parse(e)
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual s"$e)"
    assert(time < 10)
  }

  it should "follow operator precedence" in {
    val e = "a + 5 / (b + 1)"
    val start = Platform.currentTime
    val expression = Expression.parse(e)
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual e
    assert(time < 10)
  }

  it should "evaluate a simple expression" in {
    val expression = Expression.parse("(a + 2) * 3")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Left(12)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(42)
    assert(time < 10)
  }

  it should "evaluate an equality comparison" in {
    val expression = Expression.parse("a - 2 = b")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Left(6),
      "b" -> Right("4")
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(1)
    assert(time < 10)
  }

  it should "evaluate an un-equality comparison" in {
    val expression = Expression.parse("a != b")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Right("foo"),
      "b" -> Right("bar")
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(1)
    assert(time < 10)
  }

  it should "evaluate a lower than comparison" in {
    val expression = Expression.parse("a < b + 1")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Right("5"),
      "b" -> Left(5)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(1)
    assert(time < 10)
  }

  it should "evaluate a lower than or equal comparison" in {
    val expression = Expression.parse("a - 2 <= b + 1")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Left(6),
      "b" -> Left(5)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(1)
    assert(time < 10)
  }

  it should "evaluate a greater than comparison" in {
    val expression = Expression.parse("a - 2 > b + 1")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Left(6),
      "b" -> Left(5)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(0)
    assert(time < 10)
  }

  it should "evaluate a greater than or equal comparison" in {
    val expression = Expression.parse("a - 2 >= b + 1")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Left(6),
      "b" -> Left(5)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Left(0)
    assert(time < 10)
  }

  it should "evaluate a simple string concatenation" in {
    val expression = Expression.parse("a + '_' + b + '.' + c")

    val start = Platform.currentTime
    val result = expression.evaluate(RenderingData(0, Map.empty, mutable.Map[String, Either[Double, String]](
      "a" -> Right("bob"),
      "b" -> Left(5),
      "c" -> Left(4843)
    )))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    result shouldBe Right("bob_5.4843")
    assert(time < 10)
  }

  it should "simplify a numeric expression with static members" in {
    val start = Platform.currentTime
    val expression = Expression.parse("(a + 1) / (2 * 3 % 2)")
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual "(a + 1) / 2"
    assert(time < 10)
  }

  it should "simplify a string concatenation with static members" in {
    val start = Platform.currentTime
    val expression = Expression.parse("a + \"_foo_\" + 24 + \".bar\"")
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    expression.toString shouldEqual "a + \"_foo_24.bar\""
    assert(time < 10)
  }

  it should "throw an ArithmeticException when parsing a malformed expression" in {
    assert(try { Expression.parse("*"); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("+ 2"); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("a + + 2"); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("a + 2 * "); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("a + hi!"); false }
    catch { case _: ArithmeticException => true })
  }

  it should "throw an ArithmeticException when evaluating an unsupported string operation" in {
    val data = RenderingData(0, Map.empty, mutable.Map.empty[String, Either[Double, String]])

    assert(try { Expression.parse("'hi!' - 1").evaluate(data); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("'hi!' * 1").evaluate(data); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("'hi!' / 1").evaluate(data); false }
    catch { case _: ArithmeticException => true })

    assert(try { Expression.parse("'hi!' % 2").evaluate(data); false }
    catch { case _: ArithmeticException => true })
  }

  it should "throw an NoSuchElementException when a variable is missing" in {
    val data = RenderingData(0, Map.empty, mutable.Map.empty[String, Either[Double, String]])

    assert(try { Expression.parse("a * 1").evaluate(data); false }
    catch { case _: NoSuchElementException => true })
  }

}
