package fpmax

import cats.data.State
import fpmax.Console.CustomConsole
import fpmax.CustomRandom.RandomNatural
import fpmax.Fixtures.{TestData, TestState}
import org.scalatest.{FunSuite, Matchers}

class GuessingGameTest extends FunSuite with Matchers {

  test("correctFirstGuess") {
    val initial = TestData(List("Angelo", "4", "n"), List(), 4)

    val (result, _) = GuessingGame.play[TestState].run(initial).value

    result.outputs shouldEqual List(
        "What is your name?",
        "Hello, Angelo, welcome to the game!",
        "Dear Angelo, please guess a number from 1 to 5:",
        "You guessed right, Angelo!",
        "Do you want to continue, Angelo?"
    )
  }

  test("wrongGuesses") {
    val initial = TestData(List("Angelo", "4", "f", "3", "n"), List(), 5)

    val (result, _) = GuessingGame.play[TestState].run(initial).value

    result.outputs shouldEqual List(
      "What is your name?",
      "Hello, Angelo, welcome to the game!",
      "Dear Angelo, please guess a number from 1 to 5:",
      "You guessed wrong, Angelo! The number was: 5",
      "Do you want to continue, Angelo?",
      "Dear Angelo, please guess a number from 1 to 5:",
      "You guessed wrong, Angelo! The number was: 5",
      "Do you want to continue, Angelo?"
    )
  }

}

object Fixtures {

  case class TestData(inputs: List[String], outputs: List[String], num: Int)

  type TestState[A] = State[TestData, A]

  implicit val testConsole: CustomConsole[TestState] = new CustomConsole[TestState] {
    override def writeLn(msg: String): TestState[Unit] = State(testData =>
      (testData.copy(outputs = testData.outputs ++ List(msg)), Unit)
    )

    override def readLn(): TestState[String] = State(testData =>
      (testData.copy(inputs = testData.inputs.tail), testData.inputs.head)
    )
  }

  implicit val testRandomNatural: RandomNatural[TestState] = (num: Int) => State(testData =>
    (testData, testData.num)
  )

}