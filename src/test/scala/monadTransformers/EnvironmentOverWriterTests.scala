package monadTransformers

import org.scalatest.FunSuite
import ScalaMonad.typeclass2ScalaMonad

class EnvironmentOverWriterTests extends FunSuite {

  val writer = new Writer[String]
  import writer.{runWriter, tell, write, writerMonad}

  type TestEnv = Map[Int, String]
  val env = new EnvironmentTransformer[TestEnv, writer.Writer]
  import env._
  val testEnv = Map(1 -> "1", 2 -> "2", 3 -> "3")

  test("ask returns the full environment") {
    val m1 = ask
    val m2 = runEnv(m1, testEnv)
    val (v3, _) = runWriter(m2, Seq())
    assert(v3 === testEnv)
  }

  test("asks applies the environment to the provided function") {

    val m1 = asks[String]{ (e: TestEnv) => e(2) }
    val m2 = runEnv(m1, testEnv)
    val (v3, _) = runWriter(m2, Seq())
    assert(v3 === "2")
  }

  test("returnM returns the lifted value") {

    val m1 = returnM(false)
    val m2 = runEnv(m1, testEnv)
    val (v3, _) = runWriter(m2, Seq())
    assert(v3 === false)
  }

  test("bindM does the sensible thing") {

    val m1 = ask
    val m2 = bindM(m1, { (env: TestEnv) => returnM[String](env(3) + "!!!") })
    val m3 = runEnv(m2, testEnv)
    val (v4, _) = runWriter(m3, Seq())

    assert(v4 === "3!!!")
  }

  test("writer.tell can be lifted so that it can be used within the transformer") {
    val m1 = returnM(false)
    val m2 = bindM(m1, { (a: Boolean) =>
      val m1 = tell(a.toString)
      val m2 = lift[Unit](m1)
      m2
    })
    val m3 = runEnv(m2, testEnv)
    val v4 = runWriter(m3, List("Hello"))
    assert(v4 === ((), List("Hello", "false")))
  }

  test("writer.writer can be lifted so that it can be used within the transformer") {
    val m1 = returnM(false)
    val m4 = bindM(m1, { (a: Boolean) =>
      val m1 = write(a, "Pan")
      val m2 = lift[Boolean](m1)
      m2
    })
    val m5 = runEnv(m4, testEnv)
    val v6 = runWriter(m5, List("Peter"))
    assert(v6 === (false, List("Peter", "Pan")))
  }

  test("there exists a monad typeclass instance") {

    val me = implicitly[Monad[env.Env]]
    val m1 = ask
    val m2 = me.bindM(m1, { (a: TestEnv) => me.returnM(a(3)) })
    val m3 = me.bindM(m2, { (b: String) => lift[String](write(b, b))})
    val m4 = me.bindM(m3, { (b: String) => me.returnM(b + "!!!") })
    val m5 = runEnv(m4, testEnv)
    val (u, v) = runWriter(m5, Seq())
    assert(u === "3!!!")
    assert(v === Seq("3"))
  }

  test("there exists an implicit conversion to a type that supports map, flatMap") {
    val m = for {
      a <- ask
      b <- returnM[String](a(3))
      c <- lift[String](write(b, b))
    } yield (c + "!!!")

    val (u, v) = runWriter(runEnv(m, testEnv), Seq())
    assert(u === "3!!!")
    assert(v === Seq("3"))
  }

}
