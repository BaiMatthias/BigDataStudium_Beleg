object Tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


val liast = List((1, "Das der weitergeht ist ein geiler Text"),(2, "Der hier weitergeht der"),(3, "Und endet hier"))
                                                  //> liast  : List[(Int, String)] = List((1,Das der weitergeht ist ein geiler Tex
                                                  //| t), (2,Der hier weitergeht der), (3,Und endet hier))
val list1 = List("der","weitergeht")              //> list1  : List[String] = List(der, weitergeht)



 def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= {
   
 /*   l.flatMap(indexAndWords => getWords(indexAndWords._2).foldLeft(List.empty[(Int, String)]){
      (listWords, word) =>  (indexAndWords._1, word) :: listWords
     
    })*/
    
   l.flatMap(indexAndWords => getWords(indexAndWords._2).map((indexAndWords._1,_))
     
    )

  }                                               //> getAllWordsWithIndex: (l: List[(Int, String)])List[(Int, String)]
  def getWords(line:String):List[String]={
    /*
     * Extracts all words in a line
     *
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */

    line.replaceAll("[^a-zA-Z]", " ") // alle Buchstaben (gross und klein raus filtern
      .toLowerCase // toLowerCase --> alle Buchstaben in kleine
      .split(" ") // jedes Wort in eine Liste (nach Leerzeichen getrennt)
      .filter(p => p != "") // Alles raus filtern was nicht leer ist (so werden die doppelten Leerzeichen an den Stellen mit Sonderzeichen gefiltert)
      .toList // zur Rueckgabe in eine String-Liste
  }                                               //> getWords: (line: String)List[String]






def createInverseIndex(l:List[(Int,String)]):Map[String,List[Int]]={

     l.foldLeft(Map.empty[String, List[Int]]){
       (inverseMap, wordTupel) =>
        inverseMap.updated(wordTupel._2, wordTupel._1::inverseMap.getOrElse(wordTupel._2, List()))
     }
     
   }                                              //> createInverseIndex: (l: List[(Int, String)])Map[String,List[Int]]
   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={

    val lTemp =  words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()).distinct
        
     }
     
     lTemp.filter( p => lTemp.count(_ == p) == words.length).distinct.reverse
    
     
   }                                              //> andConjunction: (words: List[String], invInd: Map[String,List[Int]])List[In
                                                  //| t]
   
   createInverseIndex(getAllWordsWithIndex(liast))//> res0: Map[String,List[Int]] = Map(und -> List(3), geiler -> List(1), hier -
                                                  //| > List(3, 2), ist -> List(1), das -> List(1), weitergeht -> List(2, 1), der
                                                  //|  -> List(2, 2, 1), ein -> List(1), endet -> List(3), text -> List(1))
   
   
   
   andConjunction(list1,createInverseIndex(getAllWordsWithIndex(liast)))
                                                  //> res1: List[Int] = List(2, 1)

}