package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[O](f: T => O): MyGenericList[O]
  def foldLeft[O](init: O)(f: O => T => O): O

  def ::[O >: T](elem: O): MyGenericList[O] = MyCons(elem, this)
}

case object MyNil extends MyGenericList[Nothing] {
  def head: Nothing = undef
  def tail: MyGenericList[Nothing] = undef
  def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => this
    case _ => undef
  }
  def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => this
    case _ => undef
  }
  def map[O](f: Nothing => O): MyGenericList[O] = MyNil
  def foldLeft[O](init: O)(f: O => Nothing => O): O = init
}

case class MyCons[T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  def drop(n: Int): MyGenericList[T] = n match {
    case 0 => this
    case _ => tail.drop(n - 1)
  }
  def take(n: Int): MyGenericList[T] = n match {
    case 0 => MyNil
    case _ => head :: tail.take(n - 1)
  }
  def map[O](f: T => O): MyGenericList[O] = f(head) :: tail.map(f)
  def foldLeft[O](init: O)(f: O => T => O): O = tail.foldLeft(f(init)(head))(f)
}


object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] =
    seq.foldRight(MyNil: MyGenericList[T])((head: T, tail: MyGenericList[T]) => head :: tail)
  def size[T](myGenericList: MyGenericList[T]): Int = myGenericList.foldLeft[Int](0)(value => _ => value + 1)
}
