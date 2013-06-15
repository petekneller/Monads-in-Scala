package impls

import monadTransformers.Environment
import java.sql.Connection

object Connections {

  val env = new Environment[Connection]

}
