package fpmax

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import fpmax.Console.CustomConsole
import fpmax.CustomRandom.RandomNatural

import scala.util.Try

object GuessingGame {

  def play[F[_] : CustomConsole : RandomNatural : Monad]: F[Unit] =
    writeLn("What is your name?")
      .flatMap { _ => readLn() }
      .flatMap { name =>
        writeLn(s"Hello, $name, welcome to the game!")
          .flatMap { _ => gameLoop(name) }
      }

  def gameLoop[F[_] : CustomConsole : RandomNatural : Monad](player: String): F[Unit] =
    randomNaturalUpTo(5)
      .flatMap { num => askPlayerToGuess(player, num) }
      .flatMap { _ => writeLn(s"Do you want to continue, $player?") }
      .flatMap { _ =>
        val ifYes: () => F[Unit] = () => gameLoop(player)
        val ifNo: () => F[Unit] = () => Monad[F].point(Unit)
        checkContinue(ifNo, ifYes)
      }

  def askPlayerToGuess[F[_] : Monad: CustomConsole](player: String, num: Int): F[Option[Unit]] =
    writeLn(s"Dear $player, please guess a number from 1 to 5:")
      .flatMap { _ => readGuess() }
      .flatMap { guess => evaluateGuess(guess, num, player) }

  def readGuess[F[_] : Monad : CustomConsole](): F[Option[Int]] =
    readLn().map { input: String => Try { input.toInt }.toOption }

  def evaluateGuess[F[_]: Monad : CustomConsole](guess: Option[Int], num: Int, player: String): F[Option[Unit]] = {
    import cats.instances.option._
    import cats.syntax.traverse._

    guess
      .traverse { numberGuessed =>
        if (numberGuessed == num)
          writeLn(s"You guessed right, $player!")
        else
          writeLn(s"You guessed wrong, $player! The number was: $num")
      }
  }



  def checkContinue[F[_] : Monad: CustomConsole](ifNo: () => F[Unit], ifYes: () => F[Unit]): F[Unit] =
    readLn().flatMap { input: String =>
      input.toLowerCase() match {
        case "y" => ifYes()
        case "n" => ifNo()
        case _ => ifYes()
      }
    }

  def randomNaturalUpTo[F[_] : RandomNatural](upper: Int): F[Int] = RandomNatural[F].upTo(upper)

  def writeLn[F[_] : CustomConsole](msg: String): F[Unit] = CustomConsole[F].writeLn(msg)

  def readLn[F[_] : CustomConsole](): F[String] = CustomConsole[F].readLn()

}
