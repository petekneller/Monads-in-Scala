package monadTransformers


class Writer[O] { self =>

  // types
  type Writer[A] = Seq[O] => (A, Seq[O])

  // api
  def runWriter[A](m: Writer[A], initial: Seq[O]): (A, Seq[O]) = m(initial)

  def tell(o: O): Writer[Unit] = { (currentOutput: Seq[O]) =>
    val newOutput = currentOutput :+ o
    ((), newOutput)
  }

  def write[A](a: A, o: O): Writer[A] = { currentOutput: Seq[O] =>
    val newOutput = currentOutput :+ o
    (a, newOutput)
  }

  // monadic defn
  def returnM[A](a: A): Writer[A] = { currentOutput: Seq[O] =>
    (a, currentOutput)
  }

  def bindM[A, B](m1: Writer[A], f: (A => Writer[B])): Writer[B] = { (currentOutput: Seq[O]) =>
    val (a, outputAfterM1) = m1(currentOutput)
    val m2 = f(a)
    m2(outputAfterM1)
  }

  // typeclass instance
  implicit def writerMonad = new Monad[Writer] {
    def returnM[A](a: A): Writer[A] = self.returnM(a)
    def bindM[A, B](m: Writer[A], f: A => Writer[B]): Writer[B] = self.bindM(m, f)
  }

}
