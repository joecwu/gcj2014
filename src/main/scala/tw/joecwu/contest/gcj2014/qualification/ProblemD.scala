package tw.joecwu.contest.gcj2014.qualification

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import java.text.DecimalFormat

/**
  * Created by joewu on 4/12/14.
  * Deceitful Game
  */
object ProblemD extends Logging {
  case class Questions(q:Int, size:Int, a:List[Double], b:List[Double])

   val output = new PrintWriter(new File("2014q_resultD.out"))

   def main(args : Array[String]) {
     info("Start Fighting!!!")

//     val file = Source.fromURL(getClass().getResource("/smallD.in"))
     val file = Source.fromURL(getClass().getResource("/2014/largeD.in"))

     val reader = file.bufferedReader()

     val total = reader.readLine().toInt

     info(s"Case Count: $total")

     var i = 0;
     val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList
     qs.map{ solveQuestions(_) }.foreach{ r =>
       debug(s"Case #${r._1}: ${r._2} ${r._3}")
       output.write(s"Case #${r._1}: ${r._2} ${r._3}\r\n")
     }

     file.close()
     output.close()
   }

   def parseQuestions(i: Int, reader: BufferedReader) = {
     val size = reader.readLine().toInt
     val lineA = reader.readLine().split(' ')
     val lineB = reader.readLine().split(' ')
     Questions(i,size,lineA.map{_.toDouble}.toList,lineB.map{_.toDouble}.toList)
   }

   def solveQuestions(q:Questions) : (Int,Int,Int) = {
     val wScore = calWar(q)
     val dScore = calDeWar(q)

     (q.q, dScore, wScore)
   }

   def calWar(q:Questions) : Int = {
     val a = q.a.sortWith((x,y) => x < y )
     var b = q.b.sortWith((x,y) => x < y )
     var winCount = 0
//     debug(s"a => ${a.mkString(",")}")
//     debug(s"b => ${b.mkString(",")}")
     a.foreach{ x=>
       b.find{ y=> y>x }.map{ y =>
         // find little bigger
         b = b diff List(y)
         y
       }.getOrElse{
         // no bigger and remove smaller
         b = b.drop(1)
         winCount = winCount + 1
       }
     }
     winCount
   }

   def calDeWar(q:Questions) : Int = {
     var a = q.a.sortWith((x,y) => x < y )
     var b = q.b.sortWith((x,y) => x < y )

     var winCount = 0
     var isDone = false
     while(!isDone){
       debug(s"a => ${a.mkString(",")}")
       debug(s"b => ${b.mkString(",")}")
       isDone = true
       winCount = 0
       for( i <- 0 to a.length-1){
         if(a(i)<b(i)){
           isDone = false
         }else{
           winCount = winCount + 1
         }
       }
       if(!isDone){
         a = a.drop(1)
         b = b.take(b.length-1)
         if(a.length==0)
           isDone = true
       }
     }
     winCount
   }

 }
