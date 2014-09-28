package com.ebay.dss.main

import java.util.{Date, Locale}
import java.text.SimpleDateFormat


object testMain extends App {
val input = "123\t456\t1411916014\t 1 TNS,IND testing1 | 2 TNS,IND testing2 | 10 TNS,IND testing10 "
(new record).parse(input)
}

class record {
 def parse( s: String) = {
   val arr = s.split("\t")
   val s1 = arr(0)
   val s2 = arr(1)
   val s3:Long = java.lang.Long.parseLong(arr(2))
   val s4 = arr(3)
   val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
   val nor = s4.trim.split("\\|")
   val need = Array("1","2")
   for(a <- nor) {
     //if (a.trim.split("\\s",3)(0).equals("1") | a.trim.split("\\s",3)(0).equals("2")) {
     if ( need.contains(a.trim.split("\\s",3)(0)) ) {
    	 println("[item_id] " + s1)
    	 println("[seller_id] " + s2)
    	 println("[datetime] " + sdf.format(new java.util.Date(s3)))
    	 println("[attribute_id] " + a.trim.split("\\s",3)(0) + " : " + a.trim.split("\\s",3)(2))
    	 println()
     }
   }
 } 
}

