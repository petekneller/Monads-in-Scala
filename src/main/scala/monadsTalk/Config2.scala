package monadsTalk

import monadTransformers.Environment
import talk.Config.{ConnectionPool, someConfiguration}
import monadTransformers.ScalaMonad.typeclass2ScalaMonad

object Config2 {

  val env = new Environment[Map[String, String]]
  import env._
  def config(param: String) = env.asks{ e => e.get(param) }




  def connectionPoolBuilder = {
    for {
      host <- config("host")
      user <- config("user")
      password <- config("password")
    } yield {
      for {
        h <- host
        u <- user
        p <- password
      } yield ConnectionPool(h, u, p)
    }
  }

  val connectionPool = connectionPoolBuilder(someConfiguration)

}
