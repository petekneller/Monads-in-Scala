package monads

import collection.immutable.IndexedSeq

object Lists {

  // method 1: using map and then flatten
  val combinations = 1 to 5 map { i =>
    6 to 10 map { j =>
      11 to 15 map { k =>
        (i, j, k)
      }
    }
  }

  val results: IndexedSeq[(Int, Int, Int)] = combinations.flatten.flatten


  // method 2: using map and flatten more closely
  val combinations2 = 1 to 5 map { i =>
    6 to 10 map { j =>
      11 to 15 map { k =>
        (i, j, k)
      }
    } flatten
  } flatten



  // method 3: hiding the boilerplate with flatMap
  val combinations3 = 1 to 5 flatMap { i =>
    6 to 10 flatMap { j =>
      11 to 15 map { k =>
        (i, j, k)
      }
    }
  }

  // using the purest methods
  val combinations4 = 1 to 5 flatMap { i =>
    6 to 10 flatMap { j =>
      11 to 15 flatMap { k =>
        List((i, j, k))
      }
    }
  }

}
