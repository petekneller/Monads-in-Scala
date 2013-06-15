package monadTransformers

import org.scalatest.FunSuite
import Identity._
import ScalaMonad.typeclass2ScalaMonad

class IdentityTests extends FunSuite {

  test("returnM captures the specified value") {

    assert(runIdentity(returnM(8)) === 8)
  }

  test("bindM creates a monad that looks like function application") {

    val m1 = returnM(2)
    val m2 = bindM(m1, { (a: Int) => a * 2 })
    val m3 = bindM(m2, { (b: Int) => b * 2 })
    assert(runIdentity(m3) === 8)
  }

  test("there exists a monad typeclass instance") {

    val mc = implicitly[Monad[Id]]
    val m1 = mc.returnM(2)
    val m2 = mc.bindM(m1, { (a: Int) => a * 2 })
    val m3 = mc.bindM(m2, { (b: Int) => b * 2 })
    assert(runIdentity(m3) === 8)
  }

  test("there exists an implicit conversion to ScalaMonad") {

    val mc = for {
      a <- returnM(2)
      b <- returnM(a * 2)
    } yield (b * 2)

    assert(runIdentity(mc) === 8)
  }

}
