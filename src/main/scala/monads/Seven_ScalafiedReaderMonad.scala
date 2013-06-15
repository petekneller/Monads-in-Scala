package monads

object ScalafiedReaderMonad {

  import monads.ReaderMonad.{Reader, ReaderState}
  import monads.ReaderMonad.bindR

  // defined using flatMap, map
  trait ReaderMonad[A] extends Reader[A] { thisReader =>

    def flatMap[B](f: A => ReaderMonad[B]): ReaderMonad[B] = {
      val bindResult = bindR(thisReader, f)
      new ReaderMonad[B] {
        def apply(words: ReaderState): (B, ReaderState) = bindResult(words)
      }
    }

    def map[B](f: A => B): ReaderMonad[B] = new ReaderMonad[B] {
      def apply(origWords: ReaderState): (B, ReaderState) = {
        val (a, wordsAfterA) = thisReader.apply(origWords)
        val b = f(a)

        (b, wordsAfterA)
      }
    }
  }

  def nextWord = new ReaderMonad[String] {
    def apply(words: ReaderState) = (words.head, words.tail)
  }


  def readerMonad = for {
    word1 <- nextWord
    val nothing1 = println("The first word is: " + word1)
    word2 <- nextWord
    val nothing2 = println("The second word is: " + word2)
    word3 <- nextWord
    val nothing3 = println("And finally, the third word is: " + word3)
  } yield Unit

}
