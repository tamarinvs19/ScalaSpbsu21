package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._


sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList
  def foldLeft(function: Int => Int => Int)(init: Int): Int
}

case object IntNil extends IntList {
  def head: Int = undef
  def tail: IntList = undef

  def drop(n: Int): IntList = n match {
    case 0 => this
    case _ => undef
  }

  def take(n: Int): IntList = n match {
    case 0 => this
    case _ => undef
  }

  def map(f: Int => Int): IntList = IntNil
  def ::(elem: Int): IntList = IntCons(elem, this)
  def foldLeft(function: Int => Int => Int)(init: Int): Int = init
}

case class IntCons(head: Int, tail: IntList) extends IntList {
  def drop(n: Int): IntList = n match {
    case 0 => this
    case _ => tail.drop(n - 1)
  }

  def take(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => IntCons(head, tail.take(n - 1))
  }

  def map(f: Int => Int): IntList = IntCons(f(head), tail.map(f))

  def ::(elem: Int): IntList = IntCons(elem, this)

  def foldLeft(function: Int => Int => Int)(init: Int): Int = tail.foldLeft(function)(function(head)(init))
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case _ if seq.isEmpty => IntNil
    case _                => IntCons(seq.head, fromSeq(seq.tail))
  }

  def sum(intList: IntList): Int      = intList match {
    case IntNil => undef
    case _      => intList.foldLeft(y => _ + y) (0)
  }

  def size(intList: IntList): Int     = intList.foldLeft(_ => _ + 1)(0)
}


