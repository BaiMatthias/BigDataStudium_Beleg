object tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(58); 
  println("Welcome to the Scala worksheet");$skip(22); 

	val s="bankster	-3";System.out.println("""s  : String = """ + $show(s ));$skip(41); 
	val q=s.split("\t");System.out.println("""q  : Array[String] = """ + $show(q ));$skip(16);  //.foreach(println)
  println(q(1))}
	
}
