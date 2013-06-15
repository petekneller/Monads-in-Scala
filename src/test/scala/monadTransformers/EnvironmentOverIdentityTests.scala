package monadTransformers

import org.scalatest.FunSuite
import Identity.{Id, runIdentity, identityMonad}

class EnvironmentOverIdentityTests extends FunSuite {

  type TestEnv = Map[Int, String]
  val env = new EnvironmentTransformer[TestEnv, Identity.Id]
  import env._
  val testEnv = Map(1 -> "1", 2 -> "2", 3 -> "3")

  test("ask returns the full environment") {
    val m1 = ask
    val m2 = runEnv(m1, testEnv)
    val v3 = runIdentity(m2)
    assert(v3 === testEnv)
  }

  test("asks applies the environment to the provided function") {

    val m1 = asks[String]{ (e: TestEnv) => e(2) }
    val m2 = runEnv(m1, testEnv)
    val v3 = runIdentity(m2)
    assert(v3 === "2")
  }

  test("returnM returns the lifted value") {

    val m1 = returnM(false)
    val m2 = runEnv(m1, testEnv)
    val v3 = runIdentity(m2)
    assert(v3 === false)
  }

  test("bindM does the sensible thing") {

    val m1 = ask
    val m2 = bindM(m1, { (env: TestEnv) => returnM[String](env(3) + "!!!") })
    val m3 = runEnv(m2, testEnv)
    val v4 = runIdentity(m3)

    assert(v4 === "3!!!")
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[env.Env]]
    val m1 = ask
    val m2 = mc.bindM(m1, { (a: TestEnv) => mc.returnM(a(3)) })
    val m3 = mc.bindM(m2, { (b: String) => mc.returnM(b + "!!!") })
    val m4 = runEnv(m3, testEnv)
    assert(runIdentity(m4) === "3!!!")
  }

}
