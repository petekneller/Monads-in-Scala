package impls

import concurrent.{Await, Future, ExecutionContext}
import concurrent.duration.Duration

object Futures {

  implicit val execContext = ExecutionContext.Implicits.global

  case class MyFuture[A](fut: Future[A]) {
    def getResult: A = Await.result(fut, Duration.Inf)
  }
  
  implicit def future2MyFuture[A](fut: Future[A]): MyFuture[A] = MyFuture(fut)

}
