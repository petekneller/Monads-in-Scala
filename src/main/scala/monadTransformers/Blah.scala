package monadTransformers

class Blah {

  class Foo[A, B]
  implicit def foo[A] = new Foo[A, List[A]]
//  implicit val foo = new Foo[Boolean]
//  implicit val foo = new Foo[String]

  class Bar[A]
  implicit def bar[A](implicit a: Foo[A, List[A]]) = new Bar[A]

  val i = implicitly[Bar[String]]


  val env = new Environment[String]
  import env._//{monadInstance, environmentMonadInstance}
  val writer = new WriterTransformer[String, env.Env]
  import writer.environmentMonadInstance

  implicit def e[T] = new EnvironmentMonad[String, env.Env] {
    def ask: (String) => String = null

    def asks[A](f: (String) => A): (String) => A = null
  }

//  implicitly[EnvironmentMonad[String, env.Env]]
//  implicit val e = env.environmentMonadInstance
  implicit val w = writer.environmentMonadInstance
//  val em = implicitly[EnvironmentMonad[String, writer.Writer]]

}
