// todo: pattern matching

"123" match {
  case "123" => 123
  case _ => 0
}

val x = 123

x match {
  case x if x % 2 == 0 => 1
  case x if x % 2 != 0 => 2
  case _ => 3
}

//todo: match everywhere

List(1, 2, 3) match {
  case x :: xs =>
  case Nil =>
  case _ =>
}

val (y :: ys) = List(1, 2, 3)

//todo: name shadowing
