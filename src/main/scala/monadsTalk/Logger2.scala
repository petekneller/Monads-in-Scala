package monadsTalk

import impls.{Monad, ScalaMonad}
import ScalaMonad.typeclass2ScalaMonad
import impls.Logger2._

object Logger2 {

  // a better way?
  type Logger[A] = Seq[String] => (A, Seq[String])

  trait LoggingApi {
    def log(output: String): Logger[Unit]
    def result[A](res: A): Logger[A]
    def logWithResult[A](output: String, res: A): Logger[A]
  }
  import impls.Logger2.Logger





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
