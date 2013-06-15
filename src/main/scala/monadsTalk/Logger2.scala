package monadsTalk

import monadTransformers.{Monad, ScalaMonad}
import ScalaMonad.typeclass2ScalaMonad

object Logger2 {

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









  // a better way?
  type Logger[A] = Seq[String] => (A, Seq[String])
  object Logger {
    def log(output: String): Logger[Unit] =                                     { (orig: Seq[String]) => ((), orig :+ output) }
    def result[A](res: A): Logger[A] =                                          { (orig: Seq[String]) => (res, orig) }
    def logWithResult[A](output: String, res: A): Logger[A] =                   { (orig: Seq[String]) => (res, orig :+ output) }
  }





  def foo(aParam: String): Logger[Unit] = {
    // some stuff...

    val logger = Logger.log("done some stuff")

    // some more stuff....

    return logger
  }


  def bar(aParam: Int): Logger[Int] = {
    for {
      // some stuff
      _ <- Logger.log("done some stuff")
    } yield {
      // some more stuff that yields....
      val aResult = 3
      aResult
    }
  }


  def iDontCareAboutLoggers(aParam: Int): Logger[Int] = {
    for {
      // some stuff...
      foosResult <- foo("some guy")
      // DOES NOT use the logger
      // some more stuff
      aResult <- bar(aParam)
      // does some more stuff with aResult
    } yield aResult * 2
  }


}
