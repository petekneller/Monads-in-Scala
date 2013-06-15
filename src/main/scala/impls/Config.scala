package impls

import impls._
import monadsTalk.Config.Config

object Config {

  implicit val configIsAMonad = new Monad[Config] {
    def returnM[A](a: A): Config[A] = { (env: Map[String, String]) => Some(a) }
    def bindM[A, B](m: Config[A], f: A => Config[B]): Config[B] = { env: Map[String, String] =>
      val a = m(env)
      a match {
        case Some(x) => f(x)(env)
        case None => None
      }
    }
  }

  implicit val optionIsAMonad: Monad[Option] = new Monad[Option] {
    def returnM[A](a: A): Option[A] = Some(a)
    def bindM[A, B](m: Option[A], f: A => Option[B]): Option[B] = m flatMap f
  }

}
