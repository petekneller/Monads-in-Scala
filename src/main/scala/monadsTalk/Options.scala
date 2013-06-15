package talk

object Options {

  case class Employee(name: String, department: Option[Department])

  case class Department(name: String, director: Option[Employee])

  def findEmployee(name: String): Option[Employee] = null



  // the obvious way: but why??
  for {
    emp <- findEmployee("some guy")
    dept <- emp.department
    head <- dept.director
  } println(head.name)



  // method 1: boring
  val maybeADept = findEmployee("some guy") match {
    case Some(emp) => emp.department
    case None => None
  }

  val maybeAHead = maybeADept match {
    case Some(dept) => dept.director
    case None => None
  }

  val maybeAResult = maybeAHead match {
    case Some(head) => Some(head.name)
    case None => None
  }



  // method 2: the staircase
  val maybeAMoreConciseResult = findEmployee("some guy") match {
    case None => None
    case Some(emp) => emp.department match {
      case None => None
      case Some(dept) => dept.director match {
        case None => None
        case Some(head) => Some(head.name)
      }
    }
  }



  // hide a bit of the boilerplate
  def bind[A, B](optionA: Option[A], f: (A => Option[B])): Option[B] = optionA match {
    case None => None
    case Some(a) => f(a)
  }


  val maybeACleanerResult = bind(findEmployee("some guy"), { emp: Employee =>
    bind(emp.department, { dept: Department =>
      bind(dept.director, { head: Employee =>
        Some(head.name)
      })
    })
  })




  // add the bind to the Option
  case class BoundOption[A](capturedOption: Option[A]) {
    def bind[B](f: A => Option[B]): Option[B] = Options.bind(capturedOption, f)
  }
  implicit def optionToBoundOption[A](opt: Option[A]): BoundOption[A] = BoundOption(opt)



  val maybeAMuchCleanerResult = findEmployee("some guy") bind { emp =>
    emp.department bind { dept =>
      dept.director bind { head =>
        Some(head.name)
      }
    }
  }

}
