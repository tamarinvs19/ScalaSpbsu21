// todo: "try/catch"

try {
  throw new RuntimeException("jgal")
} catch {
  case e: RuntimeException => println(s"ex ${e.getMessage}")
}
