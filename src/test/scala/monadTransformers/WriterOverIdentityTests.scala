package monadTransformers

import org.scalatest.FunSuite
import Identity.{identityMonad, runIdentity}
import ScalaMonad.typeclass2ScalaMonad

class WriterOverIdentityTests extends FunSuite {

  val writerOverIdentity = new WriterTransformer[String, Identity.Id]
  import writerOverIdentity._

  test("returnM returns the lifted value") {

    assert(runIdentity(runWriter(returnM(false), Seq("Billy"))) === (false, Seq("Billy")))
  }

  test("tell adds its argument to the existing state") {

    assert(runIdentity(runWriter(tell("World!!"), List("Hello"))) === ((), List("Hello", "World!!")))
  }

  test("write adds to the existing state and carries a value along") {

    val m = write(2, "Pan")
    assert(runIdentity(runWriter(m, List("Peter"))) === (2, List("Peter", "Pan")))
  }

  test("bindM does the sensible thing") {

    assert(runIdentity(runWriter(bindM(returnM(false), { (b: Boolean) => write(b, "Billy") }), Seq())) === (false, Seq("Billy")))
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[Writer]]
    val m1 = mc.returnM(false)
    val m2 = mc.bindM(m1, { (a: Boolean) => write(a, "Pan") })
    val m3 = mc.bindM(m2, { (b: Boolean) => mc.returnM(b == false) })
    assert(runIdentity(runWriter(m3, Seq("Peter"))) === (true, Seq("Peter", "Pan")))
  }

  test("there exists an implicit conversion to a type that supports map, flatMap") {
    val m = for {
      a <- returnM(false)
      b <- write(a, "Pan")
    } yield b == false

    assert(runIdentity(runWriter(m, Seq("Peter"))) === (true, Seq("Peter", "Pan")))
  }

}
