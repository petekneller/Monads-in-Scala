package monadTransformers

class EnvironmentTransformer[E, M[_]: Monad] { self =>

  val innerMonad = implicitly[Monad[M]]
  type Env[A] = E => M[A]

  // api
  def runEnv[A](m: Env[A], env: E): M[A] = m(env)

  def ask: Env[E] =  { (env: E) =>
    innerMonad.returnM(env)
  }

  def asks[A](f: (E => A)): Env[A] = { (env: E) =>
    innerMonad.returnM(f(env))
  }


  // monadic defn
  def returnM[A](a: A): Env[A] = { (env: E) => innerMonad.returnM(a) }

  def bindM[A, B](ma: Env[A], f: (A => Env[B])): Env[B] = { env: E =>
    val uma = ma(env)
    val umb = innerMonad.bindM(uma, { (a: A) =>
      val mb = f(a)
      val umb = runEnv(mb, env)
      umb
    })
    umb
  }

  // transformer api
  def lift[A](m: M[A]): Env[A] = { (_: E) => m }

  // typeclass instance
  implicit def environmentMonadT = new Monad[Env] {
    def returnM[A](a: A): Env[A] = self.returnM[A](a)
    def bindM[A, B](m: Env[A], f: A => Env[B]): Env[B] = self.bindM(m, f)
  }

}
