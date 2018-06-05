import scala.io.Source
object Tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(81); 
  println("Welcome to the Scala worksheet");$skip(119); 


val liast = List((1, "Das der weitergeht ist ein geiler Text"),(2, "Der hier weitergeht der"),(3, "Und endet hier"));System.out.println("""liast  : List[(Int, String)] = """ + $show(liast ));$skip(37); 
val list1 = List("der","weitergeht");System.out.println("""list1  : List[String] = """ + $show(list1 ));$skip(15); 
print("Hallo");$skip(202); 
    
    
    
   

  
 def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= {
   
    
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
  };System.out.println("""getWords: (line: String)List[String]""");$skip(275); 



 


def createInverseIndex(l:List[(Int,String)]):Map[String,List[Int]]={

     l.foldLeft(Map.empty[String, List[Int]]){
       (inverseMap, wordTupel) =>
        inverseMap.updated(wordTupel._2, wordTupel._1::inverseMap.getOrElse(wordTupel._2, List()))
     }
     
   };System.out.println("""createInverseIndex: (l: List[(Int, String)])Map[String,List[Int]]""");$skip(360); 
   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={

    val lTemp =  words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()).distinct
        
     }
      
     lTemp.filter( p => lTemp.count(_ == p) == words.length).distinct.reverse
    
      
   };System.out.println("""andConjunction: (words: List[String], invInd: Map[String,List[Int]])List[Int]""");$skip(292); 
   
   
    
    def getDocumentGroupedByCounts(filename:String, wordCount:Int):List[(Int,List[String])]= {
    val lWords = getWords(Source.fromFile(filename).getLines.mkString("\n"))
 	   
   lWords.grouped(wordCount).zipWithIndex.map{
      l => (l._2+1, l._1)
    }.toList
  
       
  };System.out.println("""getDocumentGroupedByCounts: (filename: String, wordCount: Int)List[(Int, List[String])]""");$skip(695); 
  def analyzeSentiments(l:List[(Int,List[String])]):List[(Int, Double, Double)]= {
   
   val mSenti = getData("AFINN-111.txt").map{ line =>
   val number = ("[-]?[0-9]".r.findFirstIn(line._2)).get
   val indexOfNumber = line._2.indexOf(number)
   val word = (line._2.subSequence(0, indexOfNumber -1)).toString().trim()
    (word, number.toInt)
      }.toMap
    
    l.foldLeft(List.empty[(Int,Double,Double)]){
      (nList, tTuple) =>  (tTuple._1, tTuple._2.map(mSenti.getOrElse(_,0)).sum.toDouble,   (tTuple._2.filter(p => mSenti.get(p).map{
        ele => println(ele)
        ele >= -5 && ele <= 5
      }.getOrElse(false)).length.toDouble / tTuple._2.length)) :: nList
    }
    
   
  };System.out.println("""analyzeSentiments: (l: List[(Int, List[String])])List[(Int, Double, Double)]""");$skip(302); 
  
  
 
  def getData(filename:String):List[(Int,String)]={

    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  };System.out.println("""getData: (filename: String)List[(Int, String)]""");$skip(100); val res$0 = 
  
  analyzeSentiments(getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3));System.out.println("""res0: List[(Int, Double, Double)] = """ + $show(res$0))}
   

}
 
                /* getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3) */
   /*getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3)*/
      
                                                  
                   
   
  