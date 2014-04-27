package tw.joecwu.contest.gcj2014.round1a

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import scala.Array
import scala.math._
import scalaz._
import Scalaz._

/**
 * Created by joewu on 4/25/14.
 */
object ProblemA extends Logging {
  case class Questions(q:Int, n:Int, l:Int, init:Array[Long], target:Array[Long])


  val output = new PrintWriter(new File("2014r1a_resultA.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")
    val file = Source.fromURL(getClass().getResource("/2014/r1a/largeA.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

    qs.par.map{ q=>
      val result = solveQuestions(q)
      result
    }.toList.sortWith((a,b)=>a._1.q<b._1.q).map{ r =>
      r match {
        case (q:Questions,result:String) => {
          info(s"Q #${q.q} => ${q.n} ${q.l}")
          info(s"Case #${q.q}: ${result}")
          output.write(s"Case #${q.q}: ${result}\r\n")
        }
        case _ => error("Unknown result type.")
      }
    }

    file.close()
    output.close()
  }

  def parseQuestions(i: Int, reader: BufferedReader) = {

    val line1 = reader.readLine().split(' ')
    val init = reader.readLine().split(' ').map{java.lang.Long.parseLong(_,2)}
    val target = reader.readLine().split(' ').map{java.lang.Long.parseLong(_,2)}

    Questions(i,line1(0).toInt,line1(1).toInt,init,target)
  }

  def solveQuestions(q:Questions) : (Questions,String) = {
    debug(s"n:${q.n} l:${q.l}")

    val sortedTarget = q.target.sorted
    val sortedInit = q.init.sorted
    debug("Sorted Target:"+sortedTarget.mkString(","))
    debug("Sorted Init:"+sortedInit.mkString(","))

    // 0 case
    if(checkSame(sortedTarget,sortedInit)) return (q,"0")


    debug("Target:"+sortedTarget.map{ i => java.lang.Long.toString(i,2)+s"(${bStrToLong(java.lang.Long.toString(i,2))})"}.mkString(","))
    debug("Init  :"+sortedInit.map{ i => java.lang.Long.toString(i,2)+s"(${bStrToLong(java.lang.Long.toString(i,2))})"}.mkString(","))

    val diffArray = (for(i <- 0 to sortedTarget.length-1) yield {
      sortedTarget(i) ^ sortedInit(i)
    }).toList

    debug("XOR   :"+diffArray.map{ i => java.lang.Long.toString(i,2)+s"(${bStrToLong(java.lang.Long.toString(i,2))})"}.mkString(","))

    var count = Int.MaxValue
    // take each one as base to switch all items
    sortedTarget.foreach{ t =>
      val diff = t ^ sortedInit(0)
      if(sortedInit.map{_ ^ diff}.sorted.deep == sortedTarget.deep)
        count = min(count,bitCount(diff))
    }

    if(count.equals(Int.MaxValue))
      (q,"NOT POSSIBLE")
    else
      (q,count.toString)

    // Impossible case
//    if()


//    var o = q.init.toList.sortWith((a,b)=>a>b).toArray
////    var count = 0
//    if(checkSame(sortedTarget,o)) return "0"

//    val result = sortedTarget.map{ s =>
//      var count = Int.MaxValue
//      for(i <- 0 to o.length-1){
//        val sample = o(i)
//        val cPositions = diff(s,sample)
//        debug("diff:"+cPositions.mkString(","))
//        val result = o.map{ i =>
//          var tmp = i
//          cPositions.map{ p =>
//            tmp = updateChar(tmp,p)
//          }
//          tmp
//        }.sortWith((a,b)=>a>b).toArray
//        if(checkSame(sortedTarget,result))
//          count = min(count,cPositions.length)
//      }
//      count
//    }.sortWith((a,b)=>a<b)(0)
//
//    if(result.equals(Int.MaxValue))
//      "NOT POSSIBLE"
//    else
//      result.toString
  }

  def updateChar(s:String,p:Int) : String = {
    val c = if(s(p)=='0') '1' else '0'
//    debug(s"s:$s, p:$p, c:$c")
    if(p==0)
      c.toString + s.substring(1)
    else if(p==s.length-1){
      s.substring(0,s.length-1)+c.toString
    }else{
      s.substring(0,p)+c.toString+s.substring(p+1)
    }
  }

  def checkSame(a:Array[Long], b:Array[Long]) : Boolean = {
    a.deep == b.deep
  }

  def diffCount(a:Long, b:Long) : Int = {
    java.lang.Long.toString((a ^ b),2).count{_.equals('1')}
  }

  def bStr(a:Long) = {
    java.lang.Long.toString(a,2)
  }

  def bStrToLong(str:String) = {
    java.lang.Long.parseLong(str,2)
  }

  def bitCount(str:String) = {
    str.count{_.equals('1')}
  }

  def bitCount(a:Long) = {
    java.lang.Long.bitCount(a)
  }
}
