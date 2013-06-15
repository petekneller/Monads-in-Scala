package monadTransformers

class Environment[E] { self =>

  // types
  type Env[A] = E => A

  // api
  def runEnv[A](m: Env[A], env: E): A = m(env)

  def ask: Env[E] = identity

  def asks[A](f: (E => A)): Env[A] = { (env: E) =>
    f(env)
  }

  // monadic defn
  def returnM[A](a: A): Env[A] = { (env: E) => a }

  def bindM[A, B](m1: Env[A], f: (A => Env[B])): Env[B] = { env: E =>
    val a = m1(env)
    val m2 = f(a)
    m2(env)
  }

  // monad typeclass instance
  implicit def monadInstance = new Monad[Env] {
    def returnM[A](a: A): Env[A] = self.returnM(a)
    def bindM[A, B](m: Env[A], f: A => Env[B]): Env[B] = self.bindM(m, f)
  }


  // environment monad typeclass instance
  implicit val environmentMonadInstance = new EnvironmentMonad[E, Env] {
    def ask: Env[E]  = self.ask
    def asks[A](f: E => A): Env[A] = self.asks(f)
  }

}
