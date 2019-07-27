package fpmax

import cats.effect.IO

import scala.io.StdIn

object Console {
  trait CustomConsole[F[_]]{
    def writeLn(msg: String): F[Unit]
    def readLn(): F[String]
  }

  object CustomConsole {
    def apply[F[_]](implicit console: CustomConsole[F]): CustomConsole[F] = console

    implicit val consoleIo: CustomConsole[IO] = new CustomConsole[IO] {
      override def writeLn(msg: String): IO[Unit] = IO { println(msg)}

      override def readLn(): IO[String] = IO { StdIn.readLine()}
    }
  }
}
