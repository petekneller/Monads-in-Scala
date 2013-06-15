package monadTransformers

object Identity {

  // types
  type Id[A] = A

  // api
  def runIdentity[A](m: Id[A]): A = m

  // monadic defn
  def returnM[A](a: A): Id[A] = a

  def bindM[A, B](m: Id[A], f: (A => Id[B])): Id[B] = f(m)

  // typeclass instance
  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    def returnM[A](a: A): Id[A] = Identity.returnM(a)
    def bindM[A, B](m: Id[A], f: A => Id[B]): Id[B] = Identity.bindM(m, f)
  }

  // scala monadic defn is supplied by ScalaMonad.typeclass2ScalaMonad
}
