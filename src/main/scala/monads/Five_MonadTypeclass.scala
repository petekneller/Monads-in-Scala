package monads

object MonadTypeclass {

  // pure FP

  trait Monad[M[_]] {
    def bindM[A,B](a: M[A], f: A => M[B]): M[B]
    def returnM[C](c: C): M[C]
  }

  implicit val monadifyOption = new Monad[Option] {
    def bindM[A,B](o: Option[A], f: A => Option[B]): Option[B] = o match {
      case Some(a) => f(a)
      case None => None
    }
    def returnM[A](a: A): Option[A] = Some(a)
  }

  implicit val monadifyList = new Monad[List] {
    def bindM[A,B](l: List[A], f: A => List[B]): List[B] = {
      l map (e => f(e)) flatten
    }
    def returnM[A](a: A): List[A] = List(a)
  }


  // using instances explicitly
  val testOptionM: Option[Option[Option[String]]] = None
  val optionMonad = implicitly[Monad[Option]]
  val optionResult = optionMonad.bindM(testOptionM, { (a: Option[Option[String]]) =>
    optionMonad.bindM(a, { (b: Option[String]) =>
      optionMonad.bindM(b, { (c: String) =>
        optionMonad.returnM(c)
      })
    })
  })

  val listMonad = implicitly[Monad[List]]
  val listResult = listMonad.bindM(1 to 5 toList, { (i: Int) =>
    listMonad.bindM(6 to 10 toList, { (j: Int) =>
      listMonad.bindM(11 to 15 toList, { (k: Int) =>
        listMonad.returnM((i, j, k))
      })
    })
  })

  // using implicit evidence/helper
  def monadsWithEvidence(implicit m: Monad[List]) = {
    m.bindM(1 to 5 toList, { (i: Int) =>
      m.bindM(6 to 10 toList, { (j: Int) =>
        m.bindM(11 to 15 toList, { (k: Int) =>
          m.returnM((i, j, k))
        })
      })
    })
  }

  // context bounds
  def monadsWithContextBound[T[_] : Monad, A, B, C](t1: T[A], t2: T[B], t3: T[C]): T[C] = {
    val m = implicitly[Monad[T]]
    m.bindM(t1, { (a: A) =>
      m.bindM(t2, { (b: B) =>
        m.bindM(t3, { (c: C) =>
          m.returnM(c)
        })
      })
    })
  }




  // api for scala monads
  trait ScalaMonad[A, Self[A]] {
    def flatMap[B](f: A => Self[B]): Self[B]
    def map[B](f: A => B): Self[B]
  }

}
