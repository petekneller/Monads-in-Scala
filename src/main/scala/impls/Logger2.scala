package impls

import monadsTalk.Logger2.{Logger, LoggingApi}

object Logger2 {


  object Logger extends LoggingApi {
    def log(output: String): Logger[Unit] = { (orig: Seq[String]) => ((), orig :+ output) }
    def result[A](res: A): Logger[A] = { (orig: Seq[String]) => (res, orig) }
    def logWithResult[A](output: String, res: A): Logger[A] = { (orig: Seq[String]) => (res, orig :+ output) }
  }


  implicit def writerMonad = new Monad[Logger] {
    def returnM[A](a: A): Logger[A] = { currentOutput: Seq[String] =>
      (a, currentOutput)
    }
    def bindM[A, B](m: Logger[A], f: A => Logger[B]): Logger[B] = { (currentOutput: Seq[String]) =>
      val (a, outputAfterM1) = m(currentOutput)
      val m2 = f(a)
      m2(outputAfterM1)
    }
  }

}
