import scala.collection.mutable.ArrayBuffer

object testMain extends App {

  val input = "123\177456\1771411916014\177 1 TNS,IND testing1 testing1.1 | 2 TNS,IND testing2 test2.2 | 10 TNS,IND testing10 "
  val need = new Array[String](2)
  need(0) = "1"
  need(1) = "2"

  val output = new record(input:String,need:Array[String])
  //output.printResult
  output.parse.foreach((a:String) => println(a.replaceAll("\177","^")))
}

class record(s: String,need:Array[String]) {

  val arr = s.split("\177")
  val s1 = arr(0)
  val s2 = arr(1)
  val s3 = arr(2)
  val s4 = arr(3)

  def parse:ArrayBuffer[String] = {
    val result = ArrayBuffer[String]()
    for ( a <- parseIndexMessage(s4,need)) {
      for ( aa <- a.trim.split("\177")(1).split("\\s")) result += s1 +"\177"+ s2 + "\177"+s3 +"\177"+a.trim.split("\177")(0)+"\177" + aa
    }
    result
  }

  def parseDateTime(l : String):String = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(java.lang.Long.parseLong(l) * 1000))

  def parseIndexMessage(m: String,need:Array[String]):ArrayBuffer[String] = {
    val nor = m.trim.split("\\|")
    val result = ArrayBuffer[String]()
    nor.foreach( (a:String) =>
      if ( need.contains(a.trim.split("\\s",3)(0)) ) result += a.trim.split("\\s",3)(0) + "\177" + a.trim.split("\\s",3)(2))
    result
  }
/*
  def printResult = {
    for( a <- parseIndexMessage(s4,need)) {
      println("[item_id] " + s1)
      println("[seller_id] " + s2)
      println("[datetime] " + parseDateTime(s3))
      println("[attribute_id] " + a.replaceFirst("\177"," [value] "))
      println(" ")
    }
  }*/
}


