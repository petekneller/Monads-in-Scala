package monads

object AttemptAtLoggerMonadTraits {

  type LoggerOutput = List[String]

  trait Logger {
    def runLogger(existingOutput: LoggerOutput): LoggerOutput
  }

  // Attempt to properly decompose the monad api from logging api, logging impl

  /*
  trait LoggingMethods[Self] {
    def writeLn(toBeLogged: String): Self
  }

  import monads.MonadTypeclass.ScalaMonad

  trait LoggerM[A] extends Logger with LoggingMethods[LoggerM[A]] with ScalaMonad[A, LoggerM]

  trait LoggerMonad extends LoggerM[Unit] { outerLogger =>

    def flatMap[B](f: Unit => LoggerM[B]): LoggerMonad = new LoggerMonad {
      def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
        val outputAfterA = outerLogger.runLogger(existingOutput)
        val newLogger = f.apply(())
        newLogger.runLogger(outputAfterA)
      }
    }

    def map[B](f: Unit => B): LoggerMonad = new LoggerMonad {
      def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
        val outputAfterA = outerLogger.runLogger(existingOutput)
        val b = f.apply(())
        outputAfterA
      }
    }

    def writeLn(toBeLogged: String): LoggerMonad = new LoggerMonad {
      def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
        existingOutput :+ toBeLogged
      }
    }
  }

  val logger = new LoggerMonad {
    def runLogger(existingOutput: LoggerOutput): LoggerOutput = existingOutput
  }

  val testLoggingMonad = for {
    _ <- logger.writeLn("line 1")
    _ <- logger.writeLn("line 2")
    _ <- logger.writeLn("line 3")
  } yield { println("doing some IO in the monad map; naughty, naught"); Unit }
  */

  //testLoggingMonad.writeLn("Doesn't compile - the static type of the monad thru flatMap drops to ScalaMonad")

}
