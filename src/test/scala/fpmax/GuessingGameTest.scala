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

    result.producedOutputs shouldEqual List(
        "What is your name?",
        "Hello, Angelo, welcome to the game!",
        "Dear Angelo, please guess a number from 1 to 5:",
        "You guessed right, Angelo!",
        "Do you want to continue, Angelo?"
    )
  }

  test("wrongGuesses") {
    val initial = TestData(List("Angelo", "4", "f", "y", "s", "3", "n"), List(), 5)

    val (result, _) = GuessingGame.play[TestState].run(initial).value

    result.producedOutputs shouldEqual List(
      "What is your name?",
      "Hello, Angelo, welcome to the game!",
      "Dear Angelo, please guess a number from 1 to 5:",
      "You guessed wrong, Angelo! The number was: 5",
      "Do you want to continue, Angelo?",
      "Dear Angelo enter y/n",
      "Do you want to continue, Angelo?",
      "Dear Angelo, please guess a number from 1 to 5:",
      "Dear Angelo you have not entered a number",
      "Dear Angelo, please guess a number from 1 to 5:",
      "You guessed wrong, Angelo! The number was: 5",
      "Do you want to continue, Angelo?"
    )
  }

}

object Fixtures {

  case class TestData(givenInputs: List[String], producedOutputs: List[String], numberToGuess: Int)

  type TestState[A] = State[TestData, A]

  implicit val testConsole: CustomConsole[TestState] = new CustomConsole[TestState] {
    override def writeLn(msg: String): TestState[Unit] = State(previousState =>
      (previousState.copy(producedOutputs = previousState.producedOutputs ++ List(msg)), Unit)
    )

    override def readLn(): TestState[String] = State(previousState =>
      (previousState.copy(givenInputs = previousState.givenInputs.tail), previousState.givenInputs.head)
    )
  }

  implicit val testRandomNatural: RandomNatural[TestState] = (_: Int) => State(previousState =>
    (previousState, previousState.numberToGuess)
  )

}