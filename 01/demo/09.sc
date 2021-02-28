"Anonymous functions"

//Functional type/
val inc: Int => Int = i =>  i * i

//With explicit parameter
val inc = (i: Int) =>  i * i

//Placeholder syntax
val inc3: Int => Int = (_: Int) + 1

//partially applied function syntax
def f(a: Int, b: Int) = a * b

val x: Int => Int = f (4, _)
x(5)

