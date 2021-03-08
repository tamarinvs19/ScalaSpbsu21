package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import java.util.Comparator
import scala.math.abs

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
  def empty[T]: MyGenericList[T] = MyNil

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] =
    seq.foldRight(MyGenericList.empty[T])((head: T, tail: MyGenericList[T]) => head :: tail)

  def size[T](myGenericList: MyGenericList[T]): Int = myGenericList.foldLeft[Int](0)(value => _ => value + 1)

  def sum[T](myGenericList: MyGenericList[T])(implicit monoid: Monoid[T]): T = myGenericList match {
    case MyNil => undef
    case _     => myGenericList.foldLeft[T](monoid.mempty)(acc => x => monoid.mappend(acc, x))
  }

  def sort[T](list: MyGenericList[T])(implicit comparator: Comparator[T]): MyGenericList[T] = {
    def split(list: MyGenericList[T]): (MyGenericList[T], MyGenericList[T]) = {
      val halfLength: Int = MyGenericList.size(list) / 2
      Tuple2(list.take(halfLength), list.drop(halfLength))
    }

    def merge(list1: MyGenericList[T], list2: MyGenericList[T]): MyGenericList[T] = {
      (list1, list2) match {
        case (MyNil, _) => list2
        case (_, MyNil) => list1
        case _          =>
          if (comparator.compare(list1.head, list2.head) == 1)
            list2.head :: merge(list1, list2.tail)
          else
            list1.head :: merge(list1.tail, list2)
      }
    }

    list match {
      case MyNil                => MyNil
      case _ if size(list) == 1 => list
      case _ =>
        val (list1, list2) = split(list)
        merge(sort[T](list1), sort[T](list2))
    }
  }
}

trait Monoid[T] {
  implicit val mempty: T
  implicit def mappend(a: T, b: T): T
}

object Monoid {
  implicit final val IntMonoid: Monoid[Int] =
    new Monoid[Int] {
      override final def mappend(int1: Int, int2: Int): Int = int1 + int2
      override final val mempty: Int = 0
    }

  implicit final val StringMonoid: Monoid[String] =
    new Monoid[String] {
      override final def mappend(str1: String, str2: String): String = str1 + str2
      override final val mempty: String = ""
    }
}


object Comparator {
  implicit def MyGenericList[T](implicit elementComparator: Comparator[T]): Comparator[MyGenericList[T]] =
    (o1: MyGenericList[T], o2: MyGenericList[T]) => (o1, o2) match {
      case (MyNil, MyNil) => 0
      case (MyNil, _) => 1
      case (_, MyNil) => -1
      case (MyCons(x, xs), MyCons(y, ys)) =>
        if (x == y) MyGenericList[T].compare(xs, ys)
        else elementComparator.compare(x, y)
    }

  implicit def IntComp: Comparator[Int] =
    (int1: Int, int2: Int) => int1 compare int2

  implicit def IntCompAbs: Comparator[Int] =
    (int1: Int, int2: Int) => abs(int1) compare abs(int2)

  implicit def StringComp: Comparator[String] =
    (str1: String, str2: String) => str1 compare str2
}