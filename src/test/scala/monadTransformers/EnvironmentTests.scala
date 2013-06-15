package monadTransformers

import org.scalatest.{FunSuite}
import ScalaMonad.typeclass2ScalaMonad

class EnvironmentTests extends FunSuite {

  type TestEnv = Map[Int, String]
  val e = new Environment[TestEnv]
  import e._
  val testEnv = Map(1 -> "1", 2 -> "2", 3 -> "3")

  test("ask returns the full environment") {

    assert(runEnv(ask, testEnv) === testEnv)
  }

  test("asks applies the environment to the provided function") {

    assert(runEnv(asks{ (e: TestEnv) => e(2) }, testEnv) === "2")
  }

  test("returnM returns the lifted value") {

    assert(runEnv(returnM[Boolean](false), testEnv) === false)
  }

  test("bindM does the sensible thing") {

    assert(runEnv(bindM(ask, { (env: TestEnv) => returnM[String](env(3) + "!!!") }), testEnv) === "3!!!")
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[e.Env]]
    val m1 = ask
    val m2 = mc.bindM(m1, { (a: TestEnv) => mc.returnM(a(3)) })
    val m3 = mc.bindM(m2, { (b: String) => mc.returnM(b + "!!!") })
    assert(runEnv(m3, testEnv) === "3!!!")
  }

  test("there exists an implicit conversion to a type that supports map, flatMap") {

    val mc = for {
      a <- ask
      b <- returnM[String](a(3))
    } yield (b + "!!!")

    assert(runEnv(mc, testEnv) === "3!!!")
  }

}
