object Tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(58); 
  println("Welcome to the Scala worksheet");$skip(119); 


val liast = List((1, "Das der weitergeht ist ein geiler Text"),(2, "Der hier weitergeht der"),(3, "Und endet hier"));System.out.println("""liast  : List[(Int, String)] = """ + $show(liast ));$skip(37); 
val list1 = List("der","weitergeht");System.out.println("""list1  : List[String] = """ + $show(list1 ));$skip(360); 



 def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= {
   
 /*   l.flatMap(indexAndWords => getWords(indexAndWords._2).foldLeft(List.empty[(Int, String)]){
      (listWords, word) =>  (indexAndWords._1, word) :: listWords
     
    })*/
    
   l.flatMap(indexAndWords => getWords(indexAndWords._2).map((indexAndWords._1,_))
     
    )

  };System.out.println("""getAllWordsWithIndex: (l: List[(Int, String)])List[(Int, String)]""");$skip(707); 
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
  };System.out.println("""getWords: (line: String)List[String]""");$skip(274); 






def createInverseIndex(l:List[(Int,String)]):Map[String,List[Int]]={

     l.foldLeft(Map.empty[String, List[Int]]){
       (inverseMap, wordTupel) =>
        inverseMap.updated(wordTupel._2, wordTupel._1::inverseMap.getOrElse(wordTupel._2, List()))
     }
     
   };System.out.println("""createInverseIndex: (l: List[(Int, String)])Map[String,List[Int]]""");$skip(358); 
   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={

    val lTemp =  words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()).distinct
        
     }
     
     lTemp.filter( p => lTemp.count(_ == p) == words.length).distinct.reverse
    
     
   };System.out.println("""andConjunction: (words: List[String], invInd: Map[String,List[Int]])List[Int]""");$skip(55); val res$0 = 
   
   createInverseIndex(getAllWordsWithIndex(liast));System.out.println("""res0: Map[String,List[Int]] = """ + $show(res$0));$skip(85); val res$1 = 
   
   
   
   andConjunction(list1,createInverseIndex(getAllWordsWithIndex(liast)));System.out.println("""res1: List[Int] = """ + $show(res$1))}

}
