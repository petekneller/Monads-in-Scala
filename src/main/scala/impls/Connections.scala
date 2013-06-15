package impls

import java.sql.Connection

object Connections {

  val env = new Environment[Connection]

}
