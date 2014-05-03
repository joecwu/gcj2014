package tw.joecwu.contest.gcj2014.round1b

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import scala.Array
import scala.math._
import scalaz._
import Scalaz._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by Joe on 2014/5/3.
 */
object ProblemA extends Logging {
  case class Questions(q:Int, size:Int, strings: List[String])


  val output = new PrintWriter(new File("2014r1b_resultA.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")
    val file = Source.fromURL(getClass().getResource("/2014/r1b/largeA.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

//    qs.par.map{ q=>
   qs.map{ q=>
      val result = solveQuestions(q)
      result
    }.toList.sortWith((a,b)=>a._1.q<b._1.q).map{ r =>
      r match {
        case (q:Questions,result:String) => {
          info(s"Q #${q.q} =>\r\n${q.strings.mkString("\r\n")}")
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

    val size = reader.readLine().toInt
    val lines = {
      (for(i<-1 to size) yield reader.readLine()).toList
    }
    Questions(i,size,lines)
  }

  def solveQuestions(q:Questions) : (Questions,String) = {
    debug(s"Question:${q.q} \r\n${q.strings.mkString("\r\n")}")

    if(checkCharactors(q.strings)){
      return (q,"Fegla Won")
    }else if(q.strings.distinct.length==1){
      return (q,"0")
    }else {
//      if (q.strings.size == 2) {
//        return (q, solveTwoStrings(q.strings).toString)
//      } else {
        return (q, solveStrings(q.strings).toString)
//      }
    }

    (q,"")
  }

  def checkCharactors(strings:List[String]) = {
    val base = distinctString(strings(0))
    strings.find{ s=>
      distinctString(s) != base
    }.isDefined
  }

  def distinctString(s:String) : String = {
    s.toList.foldLeft(" ")((a,b) => {
      if(b!=a.charAt(a.length-1)){
        a+b.toString
      }else
        a
    }).drop(1)
  }

  def findGCD(s1:String,s2:String) : String = {
    var i=0
    var j=0
    var gcd=""
    while(i<s1.length && j<s2.length){
      if(s1(i)==s2(j)){
        gcd += s1(i)
        i+=1
        j+=1
      }else{
        if(s1(i)==s1(i-1)){
          i+=1
        }else{
          j+=1
        }
      }
    }
    debug(s"(s1:$s1,s2:$s2) GCE=> $gcd")
    gcd
  }

  def findLCM(s1:String,s2:String) : String = {
    var i=0
    var j=0
    var lcm=""
    while(i<s1.length && j<s2.length){
      if(s1(i)==s2(j)){
        lcm += s1(i)
        i+=1
        j+=1
      }else{
        if(s1(i)==s1(i-1)){
          lcm += s1(i)
          i+=1
        }else{
          lcm += s2(j)
          j+=1
        }
      }
      if(i>=s1.length){
        lcm+=s2.substring(j)
      }
      if(j>=s2.length){
        lcm+=s1.substring(i)
      }
    }
    debug(s"(s1:$s1,s2:$s2) LCM=> $lcm")
    lcm
  }

  def solveStrings(source_strs:List[String]) : Int = {
    val strs = source_strs.to[ListBuffer]
    var count = 0
    val base = distinctString(strs(0))
    base.toList.map{ c=>
      val innerCount = ListBuffer.fill[Int](strs.length)(0)
      for(i<-0 to strs.length-1){
        strs(i) = strs(i).dropWhile{s => if(s.equals(c)){innerCount(i)+=1;true}else{false}}
      }
      val sorted = innerCount.sorted
      val baseCount = sorted(innerCount.length/2)
      debug(s"c:$c => ${innerCount.mkString(",")} baseCount:$baseCount")
      count+=innerCount.map{ ic => math.abs(ic-baseCount) }.sum
    }
    count
  }

  def solveTwoStrings(strs:List[String]) : Int = {
    var count = 0
    val smax = strs.sortWith((s1,s2)=>s1.length>s2.length)
    var s1 = smax(0)
    var s2 = smax(strs.length-1)
    debug(s"s1:$s1 s2:$s2")
    if(s1.contains(s2))
      return (s1 diff s2).length

    val gcd = findGCD(s1,s2)
    count = s1.length+s2.length-(gcd.length*2)
    val lcm = findLCM(s1,s2)

    return count
  }

}
