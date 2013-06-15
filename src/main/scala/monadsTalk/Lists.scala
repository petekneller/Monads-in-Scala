package talk

object Lists {

  // the obvious way, but why?
  val combinations = for {
    i <- 0 to 9
    j <- 0 to 9
    k <- 0 to 9
  } yield (i, j, k)





  // method 1: using map
  val combination3 =
    0 to 9 map { (i: Int) =>
      0 to 9 map { (j: Int) =>
        0 to 9 map { (k: Int) =>
          (i, j, k)
        }
      }
    }

  val result = combination3.flatten.flatten




  // method 2: using map _with_ flatten
  val combinations3 =
    0 to 9 map { (i: Int) =>
      0 to 9 map { (j: Int) =>
        0 to 9 map { (k: Int) =>
          (i, j, k)
        }
      } flatten
    } flatten




  // hiding the boilerplate
  def bind[A, B](list: Seq[A], f: A => Seq[B]): Seq[B] = {
    list map { a: A =>
      f(a)
    } flatten
  }


  val combinations4 =
    bind((0 to 9), { (i: Int) =>
      bind((0 to 9), { (j: Int) =>
        bind((0 to 9), { (k: Int) =>
          List((i, j, k))
        })
      })
    })



  // add the bind to the Option
  case class BoundList[A](list: Seq[A]) {
    def bind[B](f: A => Seq[B]): Seq[B] = Lists.bind(list, f)
  }
  implicit def listToBoundList[A](list: Seq[A]): BoundList[A] = BoundList(list)


  val combinations5 =
    (0 to 9) bind { (i: Int) =>
      (0 to 9) bind { (j: Int) =>
        (0 to 9) bind { (k: Int) =>
          List((i, j, k))
        }
      }
    }

}
