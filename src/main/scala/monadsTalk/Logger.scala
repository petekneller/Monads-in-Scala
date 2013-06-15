package monadsTalk

object Logger {

  /*
      Logging without mutation?
   */



  // perhaps like this?
  trait Logger {
    def log(output: String): Logger
    def log(output: Seq[String]): Logger
  }

  // the obvious (but nasty) way

  def foo(aParam: String, logger: Logger): Logger = {
    // some stuff...

    val newLogger = logger.log("done some stuff")

    // some more stuff....

    return newLogger
  }


  def bar(aParam: Int, logger: Logger): (Int, Logger) = {
    // some stuff

    val newLogger = logger.log("done some stuff")

    // some more stuff that yields....
    val aResult = 3

    return (aResult, newLogger)
  }

  // this gets ugly fast
  def iDontCareAboutLoggers(aParam: Int, originalLogger: Logger): (Int, Logger) = {
    // some stuff...
    val newLogger = foo("some guy", originalLogger)

    // DOES NOT use the logger

    // some more stuff
    val (aResult, newLogger2) = bar(aParam, newLogger)

    // does some more stuff with aResult
    val anotherResult = aResult * 2


    return (anotherResult, newLogger2)
  }













}
