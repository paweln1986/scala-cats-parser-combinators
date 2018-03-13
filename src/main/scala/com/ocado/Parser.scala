package com.ocado

import com.ocado.Parser.ParseResult

case class Parser[A](parse: () => String => ParseResult[A])

object Parser{
  type ParseResult[A] = Option[(A, String)]
  def parser[A](parse: => String => ParseResult[A]) = new Parser(()=>parse)
}
