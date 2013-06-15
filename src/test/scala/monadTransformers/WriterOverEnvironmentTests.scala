package monadTransformers

import org.scalatest.FunSuite
import ScalaMonad.typeclass2ScalaMonad

class WriterOverEnvironmentTests extends FunSuite {

  type TestEnv = Map[Int, String]
  val env = new Environment[TestEnv]
  import env.{runEnv, monadInstance, environmentMonadInstance, ask, asks}
  val testEnv = Map(1 -> "1", 2 -> "2", 3 -> "3")

  val writer = new WriterTransformer[String, env.Env]
  import writer._

  test("returnM returns the lifted value") {

    assert(runEnv(runWriter(writer.returnM(false), Seq("Billy")), testEnv) === (false, Seq("Billy")))
  }

  test("tell adds its argument to the existing state") {

    assert(runEnv(runWriter(tell("World!!"), List("Hello")), testEnv) === ((), List("Hello", "World!!")))
  }

  test("write adds to the existing state and carries a value along") {

    val m = write(2, "Pan")
    assert(runEnv(runWriter(m, List("Peter")), testEnv) === (2, List("Peter", "Pan")))
  }

  test("bindM does the sensible thing") {

    assert(runEnv(runWriter(bindM(writer.returnM(false), { (b: Boolean) => write(b, "Billy") }), Seq()), testEnv) === (false, Seq("Billy")))
  }

  test("environment.ask can be lifted and used within the transformer") {

    val m1 = returnM(3)
    val m2 = bindM(m1, { (_: Int) => lift(ask) })
    val m3 = runWriter(m2, Seq())
    val m4 = runEnv(m3, testEnv)
    assert(m4 === (testEnv, Seq()))
  }

  test("environment.asks can be lifted and used within the transformer") {

    val m1 = returnM(3)
    val m2 = bindM(m1, { (_: Int) => lift(asks{ (e: TestEnv) => e(2) }) })
    val m3 = runWriter(m2, Seq())
    val m4 = runEnv(m3, testEnv)
    assert(m4 === ("2", Seq()))
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[Writer]]
    val m1 = mc.returnM(false)
    val m2 = mc.bindM(m1, { (a: Boolean) => write(a, "Pan") })
    val m3 = mc.bindM(m2, { (b: Boolean) => mc.returnM(b == false) })
    assert(runEnv(runWriter(m3, Seq("Peter")), testEnv) === (true, Seq("Peter", "Pan")))
  }

  test("there exists an implicit conversion to a type that supports map, flatMap") {
    val m = for {
      a <- returnM(false)
      b <- write(a, "Pan")
    } yield b == false

    assert(runEnv(runWriter(m, Seq("Peter")), testEnv) === (true, Seq("Peter", "Pan")))
  }
}
