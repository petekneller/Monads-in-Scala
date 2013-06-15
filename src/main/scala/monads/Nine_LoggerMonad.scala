package monads

object LoggerMonad {

  // in Scala style

  import monads.AttemptAtLoggerMonadTraits.{LoggerOutput, Logger}

  trait LoggerMonad extends Logger { outerLogger =>
    def flatMap[B](f: Unit => LoggerMonad): LoggerMonad = new LoggerMonad {
      def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
        val outputAfterA = outerLogger.runLogger(existingOutput)
        val newLogger = f()
        newLogger.runLogger(outputAfterA)
      }
    }

    def map[B](f: Unit => B): LoggerMonad = new LoggerMonad {
      def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
        val outputAfterA = outerLogger.runLogger(existingOutput)
        val junk = f()
        outputAfterA
      }
    }
  }

  def writeLn(s: String) = new LoggerMonad {
    def runLogger(existingOutput: LoggerOutput): LoggerOutput = {
      existingOutput :+ s
    }
  }

  val testLoggingMonad = for {
    _ <- writeLn("line 1")
    _ <- writeLn("line 2")
    _ <- writeLn("line 3")
  } yield { println("doing some IO in the monad map; naughty, naughty"); Unit }

}
