package fpmax

import java.io._

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}


class GuessNumberAppTest extends FunSuite with BeforeAndAfter with Matchers {

  val LS: String = System.getProperty("line.separator")

  test("correctFirstGuess") {
    val inputs = List("Angelo", "4", "n").mkString(LS)
    val outputStream = new ByteArrayOutputStream()

    scala.Console.withIn(new StringReader(inputs)) {
      scala.Console.withOut(outputStream) {
        GuessNumberApp.main(Array("2"))
      }
    }

    outputStream.toString shouldEqual
      "What is your name?\n" +
        "Hello, Angelo, welcome to the game!\n" +
        "Dear Angelo, please guess a number from 1 to 5:\n" +
        "You guessed right, Angelo!\n" +
        "Do you want to continue, Angelo?\n"
  }

  test("wrongGuesses") {
    val inputs = List("Angelo", "4", "f", "3", "n").mkString(LS)
    val outputStream = new ByteArrayOutputStream()

    scala.Console.withIn(new StringReader(inputs)) {
      scala.Console.withOut(outputStream) {
        GuessNumberApp.main(Array("3"))
      }
    }

    outputStream.toString shouldEqual
      "What is your name?\n" +
        "Hello, Angelo, welcome to the game!\n" +
        "Dear Angelo, please guess a number from 1 to 5:\n" +
        "You guessed wrong, Angelo! The number was: 5\n" +
        "Do you want to continue, Angelo?\n" +
        "Dear Angelo, please guess a number from 1 to 5:\n" +
        "You guessed wrong, Angelo! The number was: 1\n" +
        "Do you want to continue, Angelo?\n"
  }


}
