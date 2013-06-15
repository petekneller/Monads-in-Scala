package monadTransformers

class WriterTransformer[O, M[_]: Monad] { self =>

  val innerMonad = implicitly[Monad[M]]

  // types
  type Writer[A] = Seq[O] => M[(A, Seq[O])]

  // api
  def runWriter[A](m: Writer[A], initialOutput: Seq[O]): M[(A, Seq[O])] = {
    m(initialOutput)
  }

  def tell(o: O): Writer[Unit] = { (initialOutput: Seq[O]) =>
    val newOutput = initialOutput :+ o
    innerMonad.returnM(((), newOutput))
  }

  def write[A](a: A, o: O): Writer[A] = { initialOutput: Seq[O] =>
    val newOutput = initialOutput :+ o
    innerMonad.returnM((a, newOutput))
  }

  // monadic defn
  def returnM[A](a: A): Writer[A] = { (initial: Seq[O]) =>
    innerMonad.returnM((a, initial))
  }

  def bindM[A, B](ma: Writer[A], f: (A => Writer[B])): Writer[B] = { (initialOutput: Seq[O]) =>
    val innerMA = ma(initialOutput)
    val innerMB = innerMonad.bindM(innerMA, { p: (A, Seq[O]) =>
      val (a: A, outputAfterMA: Seq[O]) = p
      val mb = f(a)
      val innerMB = runWriter(mb, outputAfterMA)
      innerMB
    })
    innerMB
  }

  // transformer api
  def lift[A](m: M[A]): Writer[A] = { (initialOutput: Seq[O]) =>
    innerMonad.bindM(m, { (a: A) => innerMonad.returnM((a, initialOutput)) })
  }

  // typeclass instance
  implicit val writerMonad = new Monad[Writer] {
    def returnM[A](a: A): Writer[A] = self.returnM(a)
    def bindM[A, B](m: Writer[A], f: A => Writer[B]): Writer[B] = self.bindM(m, f)
  }


  // environment monad typeclass instance
  implicit def environmentMonadInstance[E](implicit env: EnvironmentMonad[E, M]) = new EnvironmentMonad[E, Writer] {
    def ask: Writer[E]  = lift(env.ask)
    def asks[A](f: E => A): Writer[A] = lift(env.asks(f))
  }


}
