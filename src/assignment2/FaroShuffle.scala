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

        list1 ::= ls(k)

      } else if (k <= ls.length-1){

        list2 ::= ls(k)
      }


      if( k == ls.length - 1)1
      else loop(k+1)

    }

    loop(0)

    val newList: List[List[A]] = List(list1.reverse,list2.reverse)

    println("content of list1: " + list1 + " content of list2: " + list2 + "THUG hours: " + newList)

    newList
  }

  def main(args:Array[String]): Unit = {

    val num1: List[String] = List("h1", "h2", "h3")
    val num2: List[String] = List("c1", "c2", "c3", "c4", "c5", "c6",
                                  "c7", "c8", "c9", "c10","cJ", "CQ")

    println(shuffle(num1, num2))

    println(split(num2, 6))

  }

}