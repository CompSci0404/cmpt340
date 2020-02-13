package assignment2

//Matt Radke
// 11196547
// mmr174

object FaroShuffle {

  def shuffle[A](L1:List[A], L2:List[A]): List[A] = {
    var newList = List[A]()

    //Create a function that checks the length of Both L1 and L2
    def lenChoice(x: Any) = x match {
      case x: Int if x >= L2.length => x
      case x: Int if x < L2.length => L2.length
    }

    // exception handler.
    def getListItem(ls:List[A], n:Int): Option[List[A]] = {

      // if n is bigger then the length of  list using we will return NONE
      // else we use option.
      if(ls.length > n) {
        newList ::= ls(n) //creates a NEW list, and appends the old content to the front of it.

        Some(newList)

      } else {

        None
      }

    }

    def loop(n:Int): Int = {
      getListItem(L1, n)
      getListItem(L2, n)

      if(n == (lenChoice(L1.length)-1))1
      else loop(n + 1)

    }

    loop(0)

    newList.reverse // return the final list once we are finished with it!
  }


  def split[A](ls:List[A], n:Int): List[List[A]] = {
    //var newList: List[List[A]] = List(List(), List())

    var list1  = List[A]()
    var list2 = List[A]()


    def loop(k:Int): Int = {

      if(k < n){

        list1 :+= ls(k)

      } else if (k <= ls.length-1){

        list2 :+= ls(k)
      }


      if( k == ls.length - 1)1
      else loop(k+1)

    }

    loop(0)

    val newList: List[List[A]] = List(list1,list2)

    newList
  }


  def checkDeck[A](ls:List[A], n:Int): Either[String, Int] = {

    if(ls.length % 2 != 0 ) Left("cannot have a deck that is not even!")
    else Right(n)
  }


  // already have plug in two lists  in order back and forth.
  def outShuffle[A](ls:List[A], n:Int):  List[A] = {

      var cutDeck: List[List[A]] = List()

    checkDeck(ls,n) match {
      case Left(s) => println(s)
      case Right(n) => cutDeck = split(ls,n)
    }

    shuffle(cutDeck(0), cutDeck(1))
  }

  // plug in my lists with reversal of my lists.
  def inShuffle [A](ls:List[A], n:Int): List[A] = {

    var cutDeck: List[List[A]] = List()

    checkDeck(ls,n) match {
      case Left(s) => println(s)
      case Right(n) => cutDeck = split(ls,n)
    }
    shuffle(cutDeck(1), cutDeck(0))

  }


  def nShuffle[A](f:(List[A], Int) => List[A], ls:List[A], n:Int): List[A] = {
    var newList: List[A] = ls

    def loop (k:Int): Int ={

      newList = f(newList, newList.length/2)

      //println("This is the new list: " + newList)

        if(k == n)1
        else loop(k+1)
    }

    loop(0)

    newList
  }

  def howManyShuffles[A](f:(List[A], Int)=>List[A], ls:List[A], ls2:List[A]): Int ={

    var lsShuf: List[A] = ls
    val done: Boolean = true


    def loop(count: Int): Int ={

      println(count + " yeet")

      lsShuf = f(lsShuf, lsShuf.length / 2)

      if(lsShuf == ls2) count
      else loop(count + 1)


    }

    val timesCounted = loop(0)

    timesCounted
  }

  def main(args:Array[String]): Unit = {

    val num2: List[String] = List("c1", "c2", "c3", "c4", "c5", "c6",
                                  "c7", "c8", "c9", "c10","cJ", "CQ")

    val deck: List[Int] = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
                                32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)

    val savedDeck: List[Int] = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
      32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)


    //println("out shuffle: " + outShuffle(num2, 6))
    //println("in shuffle: " + inShuffle(num2, 6))

    println(nShuffle(outShuffle[String], num2,100))

    println(nShuffle(inShuffle[String], num2, 100))

    println(howManyShuffles(outShuffle[Int],deck, savedDeck))


  }

}