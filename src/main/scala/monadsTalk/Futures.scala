package talk

import scala.concurrent.Future

object Futures {
  import impls.Futures._


  case class Employee(name: String)
  case class Department(name: String)


  def findEmployee(name: String): Future[Employee] = null
  def findDepartment(employee: Employee): Future[Department] = null
  def findHead(department: Department): Future[Employee] = null




  // the staircase
  val someGuysDepartmentHead = findEmployee("some guy") collect {
    case emp: Employee => findDepartment(emp) collect {
      case dept: Department => findHead(dept) collect {
        case head: Employee => Future(head.name)
      } getResult
    } getResult
  } getResult



  // bind
  def bind[A, B](a: Future[A], f: A => Future[B]): Future[B] = {
    a collect {
      case a: A => f(a)
    } getResult
  }


  val someGuysDepartmentHead2 =
    bind(findEmployee("some guy"), { emp: Employee =>
      bind(findDepartment(emp), { dept: Department =>
        bind(findHead(dept), { head: Employee =>
          Future(head.name)
        })
      })
    })




  // add the bind to the Option
  case class BoundFuture[A](fut: Future[A]) {
    def bind[B](f: A => Future[B]): Future[B] = Futures.bind(fut, f)
  }
  implicit def future2BoundFuture[A](fut: Future[A]): BoundFuture[A] = BoundFuture(fut)


  val someGuysDepartmentHead3 =
    findEmployee("some guy") bind { emp: Employee =>
      findDepartment(emp) bind { dept: Department =>
        findHead(dept) bind { head: Employee =>
          Future(head.name)
        }
      }
    }




  // the best way
  val someGuysDepartmentHead4 = for {
    emp <- findEmployee("some guy")
    dept <- findDepartment(emp)
    head <- findHead(dept)
  } yield head.name



}
