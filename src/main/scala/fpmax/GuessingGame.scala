package fpmax

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import fpmax.Console.CustomConsole
import fpmax.CustomRandom.RandomNatural

import scala.util.Try

object GuessingGame {

  def play[F[_] : CustomConsole : RandomNatural : Monad]: F[Unit] =
    for {
      _     <- writeLn("What is your name?")
      name  <- readLn()
      _     <- writeLn(s"Hello, $name, welcome to the game!")
      _     <- gameLoop(name)
    } yield ()

  def gameLoop[F[_] : CustomConsole : RandomNatural : Monad](player: String): F[Unit] =
    for {
      num   <- randomNaturalUpTo(5)
      _     <- askAndEvaluatePlayerGuess(player, num)
      _     <- checkContinue(player)(Monad[F].unit, gameLoop(player))
    } yield ()

  def askAndEvaluatePlayerGuess[F[_] : Monad : CustomConsole](player: String, num: Int): F[Unit] =
    for {
      guess <- readGuess(player)
      _     <- evaluateGuess(guess, num, player)
    } yield ()

  def readGuess[F[_] : Monad : CustomConsole](player: String): F[Int] =
    for {
      _     <- writeLn(s"Dear $player, please guess a number from 1 to 5:")
      input <- readLn()
      guess <- Try { Monad[F].point(input.toInt)}
                    .getOrElse(
                        writeLn(s"Dear $player you have not entered a number")
                          .flatMap { _ => readGuess(player) }
                    )
    } yield guess


  def evaluateGuess[F[_] : Monad : CustomConsole](guess: Int, num: Int, player: String): F[Unit] = {
    if (guess == num)
      writeLn(s"You guessed right, $player!")
    else
      writeLn(s"You guessed wrong, $player! The number was: $num")
  }

  def checkContinue[F[_] : Monad : CustomConsole](player: String)(ifNo: => F[Unit], ifYes: => F[Unit]): F[Unit] =
    for {
      _     <- writeLn(s"Do you want to continue, $player?")
      ans   <- readLn()
      _     <- ans.toLowerCase() match {
                  case "y" => ifYes
                  case "n" => ifNo
                  case _ => writeLn(s"Dear $player enter y/n").flatMap { _ => checkContinue(player)(ifNo, ifYes)}
               }
    } yield ()


  def randomNaturalUpTo[F[_] : RandomNatural](upper: Int): F[Int] = RandomNatural[F].upTo(upper)

  def writeLn[F[_] : CustomConsole](msg: String): F[Unit] = CustomConsole[F].writeLn(msg)

  def readLn[F[_] : CustomConsole](): F[String] = CustomConsole[F].readLn()

}
