package monadsTalk

import anorm._
import java.sql.Connection
import monadTransformers.Environment
import monadTransformers.ScalaMonad.typeclass2ScalaMonad

object Connections {

//  def getMeEverything = SQL("select * from everything").apply()




  def getMeEverything2(implicit conn: Connection) = SQL("select * from everything").apply()





  def doesSomeStuffAndAsksForEverything(arg1: Int, arg2: Boolean): Int = {
    // does some stuff...

    val rows = getMeEverything2

    // does some more stuff

    return rows.size
  }








  val env = new Environment[Connection]
  import env._

  def mySQL(stmt: String): env.Env[Stream[SqlRow]] = asks{ implicit c: Connection => SQL(stmt).apply() }





  def getMeEverything3 = mySQL("select * from everything")






  def doesSomeStuffAndAsksForEverything2(arg1: Int, arg2: Boolean): env.Env[Int] = {
    for {
    // does some stuff...

      rows <- getMeEverything3
//      rows2 <- ordersFor(LocalDate.now)

    // does some more stuff
    } yield rows.size
  }

  val conn: Connection = null
  val theNumberOfRows = doesSomeStuffAndAsksForEverything2(1,2)(conn)



}
