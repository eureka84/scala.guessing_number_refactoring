package fpmax

import cats.effect.IO

import scala.util.Random

object CustomRandom {
  trait RandomNatural[F[_]]{
    def upTo(num: Int): F[Int]
  }

  object RandomNatural {
    def apply[F[_]](implicit randomNatural: RandomNatural[F]): RandomNatural[F] = randomNatural

    implicit def randomNaturalIO(implicit random: Random): RandomNatural[IO] = (num: Int) => IO {
      random.nextInt(num) + 1
    }
  }
}
