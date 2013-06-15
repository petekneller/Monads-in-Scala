package monadTransformers

import org.scalatest.FunSuite
import ScalaMonad.typeclass2ScalaMonad

class WriterTests extends FunSuite {

  val writer = new Writer[String]
  import writer._

  test("tell adds its argument to the existing state") {

    assert(runWriter(tell("World!!"), List("Hello")) === ((), List("Hello", "World!!")))
  }

  test("write adds to the existing state and carries a value along") {

    val m = write(2, "Pan")
    assert(runWriter(m, List("Peter")) === (2, List("Peter", "Pan")))
  }

  test("returnM returns the lifted value") {

    assert(runWriter(returnM[Boolean](false), Seq("Billy")) === (false, Seq("Billy")))
  }

  test("bindM does the sensible thing") {

    assert(runWriter(bindM(returnM[Boolean](false), { (b: Boolean) => write(b, "Billy") }), Seq()) === (false, Seq("Billy")))
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[Writer]]
    val m1 = mc.returnM(false)
    val m2 = mc.bindM(m1, { (a: Boolean) => write(a, "Pan") })
    val m3 = mc.bindM(m2, { (b: Boolean) => mc.returnM(b == false) })
    assert(runWriter(m3, Seq("Peter")) === (true, Seq("Peter", "Pan")))
  }

  test("there exists an implicit conversion to a type that supports map, flatMap") {
    val m = for {
      a <- returnM(false)
      b <- write(a, "Pan")
    } yield b == false

    assert(runWriter(m, Seq("Peter")) === (true, Seq("Peter", "Pan")))
  }

}
