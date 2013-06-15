package monads

object Options {

  // Options
  class Foo(val barO: Option[Bar] = None)

  class Bar(val bazO: Option[Baz] = None)

  type Baz = String


  // Usage
  val fooO: Option[Foo] = Some(new Foo)

  // the obvious way: but why??
  for {
    foo <- fooO
    bar <- foo.barO
    baz <- bar.bazO
  } println(baz)



  // method 1: boring
  val maybeABar = fooO match {
    case Some(foo) => foo.barO
    case None => None
  }

  val maybeABaz = maybeABar match {
    case Some(bar) => bar.bazO
    case None => None
  }

  val maybeAResult = maybeABaz match {
    case Some(baz) => Some("Result!!")
    case None => None
  }



  // method 2: the staircase
  val maybeAQuickerResult = fooO match {
    case None => None
    case Some(foo) => foo.barO match {
      case None => None
      case Some(bar) => bar.bazO match {
        case None => None
        case Some(baz) => Some("Result!!")
      }
    }
  }


  // method 3: hide a bit of the boilerplate
  def bind[A, B](optionA: Option[A], f: (A => Option[B])): Option[B] = optionA match {
    case None => None
    case Some(a) => f(a)
  }

  val maybeACleanerResult = bind(fooO, { foo: Foo =>
    bind(foo.barO, { bar: Bar =>
      bind(bar.bazO, { baz: Baz =>
        Some("Result!!")
      })
    })
  })


  // method 4: add the bind to the Option
  trait BoundOption[A] {
    val option: Option[A]
    def bindTo[B](f: A => Option[B]): Option[B] = bind(option, f)
  }

  implicit def optionToBoundOption[A](o: Option[A]): BoundOption[A] = new BoundOption[A] { val option = o }

  val maybeAMuchCleanerResult = fooO bindTo { foo =>
    foo.barO bindTo { bar =>
      bar.bazO bindTo { baz =>
        Some("Result!!")
      }
    }
  }

  // method 5: using flatMap
  val maybeAFlatMappedResult = fooO flatMap { foo =>
    foo.barO flatMap { bar =>
      bar.bazO flatMap { baz =>
        Some("Result!!")
      }
    }
  }



}
