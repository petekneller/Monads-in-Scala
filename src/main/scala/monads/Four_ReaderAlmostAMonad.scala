package monads


object ReaderAlmostAMonad {


  trait Reader {
    def nextChar(): (Char, Reader)
    def nextWord(): (String, Reader)
    def everything(): (List[String], Reader)
  }


  val input: List[String] = "Hello world from inside the Reader Monad".split(" ").toList

  case class ListBackedReader(words: List[String]) extends Reader {
    def nextChar() = {
      val firstWord = words.head
      val char = firstWord.head
      val restOfFirstWord = firstWord.tail

      (char, ListBackedReader(restOfFirstWord :: words.tail))
    }
    def nextWord() = (words.head, ListBackedReader(words.tail))
    def everything() = (words, ListBackedReader(List.empty))
  }


  val initialReader: Reader = ListBackedReader(input)


  // method 1: pattern matching to decompose the reader results; pass the reader around explicitly
  def patternMatching() {
    initialReader.nextWord() match {
      case (word1: String, reader2: Reader) => {
        println("The first word is: " + word1)
        reader2.nextWord() match {
          case (word2: String, reader3: Reader) => {
            println("The second word is: " + word2)
            reader3.nextWord() match {
              case (word3: String, reader4: Reader) => {
                println("And finally, the third word is: " + word3)
              }
            }
          }
        }
      }
    }
  }



  // method 2: bind the computations together, but keep the reader explicit
  trait ReaderState[A] {
    val state: (A, Reader)
    def bindR[B](f: (A, Reader) => (B, Reader)): (B, Reader) = {
      val (a: A, origReader: Reader) = state
      f(a, origReader)
    }
  }

  implicit def monadifyReaderState[A](in: (A, Reader)) = new ReaderState[A] { val state = in }

  def monadWithExplicitReader() {
    initialReader.nextWord() bindR { (word1: String, reader2: Reader) => {
        println("The first word is: " + word1)
        reader2.nextWord() bindR { (word2: String, reader3: Reader) => {
            println("The second word is: " + word2)
            reader3.nextWord() bindR { (word3: String, reader4: Reader) => {
                println("And finally, the third word is: " + word3)
                (Nil, reader4)
              }
            }
          }
        }
      }
    }
  }



}
