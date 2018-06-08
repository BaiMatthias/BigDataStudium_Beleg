package wordcount
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.JFreeChart
import org.jfree.ui.ApplicationFrame
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYSplineRenderer
import scala.io.Source


/**
 * @author hendrik
 */
class Sentiments (sentiFile:String){
 
  val sentiments:Map[String,Int]= getSentiments(sentiFile)
  
  val proc= new Processing()
  
/**********************************************************************************************
   *
   *                          Aufgabe 5
   *   
   *********************************************************************************************
  */

  def getDocumentGroupedByCounts(filename:String, wordCount:Int):List[(Int,List[String])]= {
    val wMap = Processing.getData(filename)

    wMap.flatMap(a => proc.getWords(a._2 )).grouped(wordCount).zipWithIndex.map{ // Erstelle eine neue Liste und gruppiere diese anhand
      // vorgegebenem WordCount und Index anhaengen
      l => (l._2+1, l._1) // Attribute noch umdrehen
    }.toList
  }
  
                                                          // Abschnitt, Sentimentwert, Relative Anzahl von genutzten Woertern
  def analyzeSentiments(l:List[(Int,List[String])]):List[(Int, Double, Double)]= {
    l.foldLeft(List.empty[(Int,Double,Double)]){ // Erzeuge das Tripel - so geschrieben?
      (nList, tTuple) =>  (tTuple._1, (tTuple._2.map(sentiments.getOrElse(_,0)).sum.toDouble / tTuple._2.filter(f => sentiments.get(f).map{
        ele => ele >= -5 && ele <= 5
      }.getOrElse(false)).length),


        (tTuple._2.filter(p => sentiments.get(p).map{
          ele => ele >= -5 && ele <= 5
        }.getOrElse(false)).length.toDouble / tTuple._2.length)) :: nList
    }.reverse
    // Erstes Element ist der Absatz, zweites Element ist das Auslesen der Map mit den Woertern, um Sentimentwert zu bekommen
    // Wenn das Wort nicht in der Map ist, einfach +0 addieren, aendert nix am Wert
    // Drittes Element zaehlt, wieviele Woerter in der Map gefunden wurden und teilt das durch die Gesamtzahl der Woerter aus der Quelle
  }

  
  /**********************************************************************************************
   *
   *                          Helper Functions
   *   
   *********************************************************************************************
  */
  
  def getSentiments(filename:String):Map[String,Int]={
        
    val url=getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    val result:Map[String,Int]= (for (row <- iter) yield {val seg= row.split("\t"); (seg(0) -> seg(1).toInt)}).toMap
    src.close()
    result
  }
  
  
  def createGraph(data:List[(Int,Double,Double)]):Unit={
    
    val series1:XYSeries = new XYSeries("Sentiment-Werte");
    for (el <- data){series1.add(el._1,el._2)}
    val series2:XYSeries  = new XYSeries("Relative Haeufigkeit der erkannten Worte");
    for (el <- data){series2.add(el._1,el._3)}
    
    val dataset1:XYSeriesCollection  = new XYSeriesCollection();
    dataset1.addSeries(series1);
    val dataset2:XYSeriesCollection  = new XYSeriesCollection();
    dataset2.addSeries(series2);
    
    val dot:XYDotRenderer = new XYDotRenderer();
    dot.setDotHeight(5);
    dot.setDotWidth(5);
    
    val spline:XYSplineRenderer = new XYSplineRenderer();
    spline.setPrecision(10);
    
    val x1ax:NumberAxis= new NumberAxis("Abschnitt");
    val y1ax:NumberAxis = new NumberAxis("Sentiment Werte");
    val x2ax:NumberAxis= new NumberAxis("Abschnitt");
    val y2ax:NumberAxis = new NumberAxis("Relative Haeufigfkeit");
    
    val plot1:XYPlot  = new XYPlot(dataset1,x1ax,y1ax, spline);
    val plot2:XYPlot  = new XYPlot(dataset2,x2ax,y2ax, dot);

    val chart1:JFreeChart  = new JFreeChart(plot1);
    val chart2:JFreeChart  = new JFreeChart(plot2);
    val frame1:ApplicationFrame = new ApplicationFrame("Sentiment-Analyse: Sentiment Werte"); 
    val frame2:ApplicationFrame = new ApplicationFrame("Sentiment-Analyse: Relative Häufigkeiten"); 
    val chartPanel1: ChartPanel = new ChartPanel(chart1);
    val chartPanel2: ChartPanel = new ChartPanel(chart2);
    
    frame1.setContentPane(chartPanel1);
    frame1.pack();
    frame1.setVisible(true);
    frame2.setContentPane(chartPanel2);
    frame2.pack();
    frame2.setVisible(true);
  }
  
  
  
  
}
