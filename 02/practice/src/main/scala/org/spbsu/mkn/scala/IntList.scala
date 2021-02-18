package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._


sealed trait IntList {
  def head: Int
  def tail: IntList
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

  def foldLeft(function: Int => Int => Int)(init: Int): Int = this match {
    case IntNil => init
    case _ => this.tail.foldLeft(function)(function(init)(head))
  }
}

case object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = undef
  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(head: Int, tail: IntList) extends IntList {}

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

  def size(intList: IntList): Int     = intList match {
    case IntNil => 0
    case _      => 1 + size(intList.tail)
  }
}


