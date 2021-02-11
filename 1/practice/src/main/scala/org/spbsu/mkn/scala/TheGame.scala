package org.spbsu.mkn.scala

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random.alphanumeric

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var numberList: List[Char] = List()
    while (numberList.length < length) {
      val nextSymbol = alphanumeric.take(1)
      if (!numberList.contains(nextSymbol) && "0123456789".contains(nextSymbol.head))
        numberList = numberList.concat(nextSymbol)
    }
    numberList.mkString("")
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    else if (userInput.length != userInput.toSet.size)
      throw new RepeatingDigitsException
    else if (secret == userInput)
      Correct(numTries)
    else {
      val bulls = (secret zip userInput).count(pair => pair._1 == pair._2)
      val cows = userInput.count(secret.contains(_)) - bulls
      Incorrect(bulls, cows)
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    print("Enter the length of the string: ")
    val length = readLine().toInt
    val secret = TheGame.generateNumberString(length)

    println("I am ready!")

    @tailrec
    def userGuess(numTries: Int = 1): Unit = {
      print("Enter your guess: ")
      val guess = readLine()
      val result: GuessResult = TheGame.validate(secret, guess)
      result match {
        case Correct(numTries) =>
          println(s"You win! Number of tries: $numTries.")
        case Incorrect(bulls, cows) =>
          println(s"Bulls: $bulls, Cows: $cows. Try again...")
          userGuess(numTries + 1)
      }
    }
    userGuess()
  }
}
