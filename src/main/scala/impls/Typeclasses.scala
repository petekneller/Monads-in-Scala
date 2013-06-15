package impls

trait Monad[M[_]] {
  def returnM[A](a: A): M[A]
  def bindM[A, B](a: M[A], f: A => M[B]): M[B]
}

trait ScalaMonad[A, T[_]] {
  def map[B](f: A => B): T[B]
  def flatMap[B](f: A => T[B]): T[B]
}

object ScalaMonad {

  implicit def typeclass2ScalaMonad[A, M[_]: Monad](m: M[A]): ScalaMonad[A, M] = new ScalaMonad[A, M] {
    val monad = implicitly[Monad[M]]
    def map[B](f: A => B): M[B] = monad.bindM(m, { (a: A) => monad.returnM(f(a))})
    def flatMap[B](f: A => M[B]): M[B] = monad.bindM(m, f)
  }

}
