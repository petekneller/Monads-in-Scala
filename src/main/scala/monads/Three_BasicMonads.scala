package monads

object BasicMonads {
  import Options._

  // Option
  val res1 = fooO flatMap { foo =>
    foo.barO flatMap { bar =>
      bar.bazO flatMap { baz =>
        Some("Result!!")
      }
    }
  }

  // List
  val res2 = 1 to 5 flatMap { i =>
    6 to 10 flatMap { j =>
      11 to 15 flatMap { k =>
        List((i, j, k))
      }
    }
  }


  // Hiding the creation of the yielded value
  def yieldO[A](a: A): Option[A] = Some(a)
  def yieldL[A](a: A): List[A] = List(a)

  val res3 = fooO flatMap { foo =>
    foo.barO flatMap { bar =>
      bar.bazO flatMap { baz =>
        yieldO("Result!!")
      }
    }
  }

  val res4 = 1 to 5 flatMap { i =>
    6 to 10 flatMap { j =>
      11 to 15 flatMap { k =>
        yieldL((i, j, k))
      }
    }
  }


  // the right way
  val res5 = for {
    foo <- fooO
    bar <- foo.barO
    baz <- bar.bazO
  } yield "Result!!"

  val res6 = for {
    i <- 1 to 5
    j <- 6 to 10
    k <- 11 to 15
  } yield (i, j, k)

}
