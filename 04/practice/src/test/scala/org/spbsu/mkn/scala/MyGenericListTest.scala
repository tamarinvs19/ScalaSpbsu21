package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sort, sum}


class MyGenericListTest extends AnyFunSuite {

  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert(MyNil.map((x: Int) => x * 2) == MyNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum Int") {
    assertThrows[UnsupportedOperationException](sum[Int](MyNil))
    assert(sum[Int](fromSeq(Seq(1,2,3))) == 6)
    assert(sum[Int](fromSeq(Seq(1))) == 1)
  }

  test("sum String") {
    assertThrows[UnsupportedOperationException](sum[String](MyNil))
    assert(sum[String](fromSeq(Seq("1, ","2, ","3"))) == "1, 2, 3")
    assert(sum[String](fromSeq(Seq("1"))) == "1")
  }

  test("sort StringList") {
    import org.spbsu.mkn.scala.Comparator.StringComp

    assert(sort(MyNil) == MyNil)
    assert(sort(fromSeq(Seq("abc", "aaa", "zzzzzzz"))) == fromSeq(Seq("aaa", "abc", "zzzzzzz")))
    assert(sort(fromSeq(Seq("aaa"))) == fromSeq(Seq("aaa")))
  }

  test("sort IntList") {
    import org.spbsu.mkn.scala.Comparator.IntComp

    assert(sort(MyNil) == MyNil)
    assert(sort(fromSeq(Seq(3, 1000, 2))) == fromSeq(Seq(2, 3, 1000)))
    assert(sort(fromSeq(Seq(1))) == fromSeq(Seq(1)))
    assert(sort(fromSeq(Seq(3, 1000, 2, 1, 239, 100500, 0, -10, 3, 4)))
      == fromSeq(Seq(-10, 0, 1, 2, 3, 3, 4, 239, 1000, 100500)))
  }

  test("sort IntList abs") {
    import org.spbsu.mkn.scala.Comparator.IntCompAbs

    assert(sort(MyNil) == MyNil)
    assert(sort(fromSeq(Seq(3, 1000, 2))) == fromSeq(Seq(2, 3, 1000)))
    assert(sort(fromSeq(Seq(1))) == fromSeq(Seq(1)))
    assert(sort(fromSeq(Seq(3, 1000, 2, 1, 239, 100500, 0, -10, 3, 4)))
      == fromSeq(Seq(0, 1, 2, 3, 3, 4, -10, 239, 1000, 100500)))
  }

  test("sort List of IntList") {
    import org.spbsu.mkn.scala.Comparator.{IntComp, MyGenericList}

    assert(sort(fromSeq(Seq(fromSeq(Seq(2, 3, 1000)), fromSeq(Seq(1)))))
      == fromSeq(Seq(fromSeq(Seq(1)), fromSeq(Seq(2, 3, 1000)))))
  }

}
