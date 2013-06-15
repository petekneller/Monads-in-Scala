package monadTransformers

import org.scalatest.FunSuite

class EnvironmentClassTests extends FunSuite {

  type TestEnv = Map[Int, String]
  val env = new Environment[TestEnv]
  import env._
  val testEnv = Map(1 -> "1", 2 -> "2", 3 -> "3")

  def asksForEnvironments[E, M[_], A](m: M[A])(implicit env: EnvironmentMonad[E, M], monad: Monad[M]): M[E] = {
    monad.bindM(m, { (_: A) => env.ask })
  }

  test("environment type implements environment monad") {

    val m1 = returnM[Boolean](false)
    val m2 = asksForEnvironments[TestEnv, Env, Boolean](m1)
    assert(runEnv(m2, testEnv) === testEnv)
  }

  test("writer transformer implements environment monad where underlying type also does") {

    val writer = new WriterTransformer[String, Env]
    import writer._
    implicit val em = env.environmentMonadInstance // not sure quite why this is necessary

    val m1 = writer.returnM(false)
    val m2 = asksForEnvironments[TestEnv, Writer, Boolean](m1)
    assert(runEnv(runWriter(m2, Seq()), testEnv) === (testEnv, Seq()))
  }

}
