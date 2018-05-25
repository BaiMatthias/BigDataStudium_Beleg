package wordcount

/**
 * @author hendrik
 */
object App {
  
 
  /*
   * 
   *   Diese Anwendung fuehrt eine Sentiment-Analyse mit 
   *   dem Buch "Vom Winde verweht" durch
   *   
   *   Es werden zwei Graphen erstellt. Der eine beinhaltet die Stimmung 
   *   in den über 20000 Woertern erzeugten Absaetzen.
   *   Der zweite Graph enthaelt die relative Haeufigkeit der erkannten Woerter,
   *   d.h. die Anzahl der Woerter, die für die Analyse der Stimmung herangezogen 
   *   wurden.
   *    
   *  
   *  Testen Sie die App mit verschiedenen Absatzgroessen
   */
  def main(args: Array[String]) = {
    
    val sentiAnalyse= new Sentiments("AFINN-112.txt")
    val book= sentiAnalyse.getDocumentGroupedByCounts("GoneWithTheWind.txt", 20000)
    val data= sentiAnalyse.analyzeSentiments(book)
    sentiAnalyse.createGraph(data)  
  }

}