package monadTransformers

trait Monad[M[_]] {
  def returnM[A](a: A): M[A]
  def bindM[A, B](a: M[A], f: A => M[B]): M[B]
}

trait ScalaMonad[A, T[_]] {
  def map[B](f: A => B): T[B]
  def flatMap[B](f: A => T[B]): T[B]
}

object ScalaMonad {

  // currently doesn't work for types that have more than one parameter (like Writer, Env...); not sure why
  implicit def typeclass2ScalaMonad[A, M[_]: Monad](m: M[A]): ScalaMonad[A, M] = new ScalaMonad[A, M] {
    val monad = implicitly[Monad[M]]
    def map[B](f: A => B): M[B] = monad.bindM(m, { (a: A) => monad.returnM(f(a))})
    def flatMap[B](f: A => M[B]): M[B] = monad.bindM(m, f)
  }

//  implicit def function2ScalaMonad[A, E](f: E => A): ScalaMonad[A, ({type L[X] = E => X})#L] = new ScalaMonad[A, ({type L[X] = E => X})#L] {
//    type F[X] = E => X
//
//    def map[B](f2: A => B): F[B] = { (e: E) =>
//      val a = f(e)
//      f2(a)
//    }
//    def flatMap[B](f2: A => F[B]): F[B] = { (e: E) =>
//      val a = f(e)
//      val fb = f2(a)
//      fb(e)
//    }
//  }

}

trait EnvironmentMonad[E, M[_]] {
  def ask: M[E]
  def asks[A](f: (E => A)): M[A]
}
