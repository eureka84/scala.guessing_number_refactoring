package fpmax

import cats.effect.IO

import scala.util.{Random, Try}

object GuessNumberApp {
  def main(args: Array[String]): Unit = {
    implicit val random: Random = Try(new Random(args(0).toInt)).getOrElse(new Random())

    GuessingGame.play[IO].unsafeRunSync()
  }
}
