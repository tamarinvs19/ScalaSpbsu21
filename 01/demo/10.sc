import java.io.File

//Local functions
def foo(): Unit = {
  def bar() = {

  }
  bar()
}

//More about functions:

//todo: repeated parameters

def foo1(a: Int*) = a
foo1(1, 2, 3)

//todo: named parameters

//todo: default arguments

//todo: by-name parameters
// ленивые параметры

def foo2(a: =>String): Unit = {
  println("foo2")
  println(a)
}

def str = {
  println("str")
  "foo"
}

foo2(str)

//todo: currying

def withResources(f: File)(body : File => Unit)

val functionToUnit = withResources(???)(_)
functionToUnit { f =>
  f.deleteOnExit()
}