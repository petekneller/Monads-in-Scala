package monads

object ReaderMonad {

  // Haskell style!

  // types
  type ReaderState = List[String]

  type Reader[A] = ReaderState => (A, ReaderState)


  // api
  def nextWord: Reader[String] = { words: ReaderState =>
    (words.head, words.tail)
  }

  def nextChar: Reader[Char] = { words: ReaderState =>
    val firstWord = words.head
    val char = firstWord.head
    val restOfFirstWord = firstWord.tail

    (char, restOfFirstWord :: words.tail)
  }

  def everything: Reader[List[String]] = { words: ReaderState =>
    (words, List.empty)
  }



  def bindR[A, B](r: Reader[A], f: A => Reader[B]): Reader[B] = {
    { origWords : ReaderState =>
      val (a, wordsAfterA) = r(origWords)
      val readerB = f(a)
      val (b, wordsAfterB) = readerB(wordsAfterA)

      (b, wordsAfterB)
    }
  }

  // method 1: explicit use
  val initialState = monads.ReaderAlmostAMonad.input

  def readerBinding() = {
    bindR(nextWord, { word1: String => {
      println("The first word is: " + word1)
      bindR(nextWord, { word2: String => {
        println("The second word is: " + word2)
        bindR(nextWord, { word3: String => {
          println("And finally, the third word is: " + word3)
          readNothing
        }
        })
      }
      })
    }
    })
  }

  def readNothing: Reader[Unit] = { words: ReaderState => (Unit, words) }


  // method 2: typeclass and implicits
  import monads.MonadTypeclass.Monad

  implicit val monadifyReader = new Monad[Reader] {
    def bindM[A, B](r: Reader[A], f: A => Reader[B]): Reader[B] = bindR(r, f)
    def returnM[B](b: B) = { words: ReaderState => (b, words) }
  }

  def testReaderMonad = {
    val m = implicitly[Monad[Reader]]
    m.bindM(nextWord, { word1: String => {
      println("The first word is: " + word1)
      m.bindM(nextWord, { word2: String => {
        println("The second word is: " + word2)
        m.bindM(nextWord, { word3: String => {
          println("And finally, the third word is: " + word3)
          m.returnM(Unit)
        }
        })
      }
      })
    }
    })
  }



}