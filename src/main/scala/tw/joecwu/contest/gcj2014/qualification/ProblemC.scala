package tw.joecwu.contest.gcj2014.qualification

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import java.text.DecimalFormat
import scala.math._
import scala.collection.mutable.ArrayBuffer

/**
  * Created by joewu on 4/12/14.
  * Minesweeper Master
  */
object ProblemC extends Logging {
  val DEBUG = false

  case class Questions(q:Int, r:Int, c:Int, m:Int)

  val output = new PrintWriter(new File("2014q_resultC.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")

//    val file = Source.fromURL(getClass().getResource("/smallC.in"))
    val file = Source.fromURL(getClass().getResource("/2014/q/largeC.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList
    qs.map{ solveQuestions(_) }.foreach{ r =>
      if(r._1){
        debug(s"Case #${r._2.q}:\r\n"+printSquare(r._2.r,r._2.c,r._2.m))
        if(DEBUG)
          output.write(s"Case #${r._2.q} - (${r._2.r},${r._2.c},${r._2.m}):\r\n"+printSquare(r._2.r,r._2.c,r._2.m)+"\r\n")
        else
          output.write(s"Case #${r._2.q}:\r\n"+printSquare(r._2.r,r._2.c,r._2.m)+"\r\n")
      }
      else{
        debug(s"Case #${r._2.q}:\r\nImpossible")
        if(DEBUG)
          output.write(s"Case #${r._2.q} - (${r._2.r},${r._2.c},${r._2.m}) => ${r._2.r*r._2.c-r._2.m}:\r\nImpossible\r\n")
        else
          output.write(s"Case #${r._2.q}:\r\nImpossible\r\n")
      }
    }


    file.close()
    output.close()
  }

  def parseQuestions(i: Int, reader: BufferedReader) = {
    val line = reader.readLine().split(' ')
    Questions(i,line(0).toInt,line(1).toInt,line(2).toInt)
  }

  def solveQuestions(q:Questions) : (Boolean, Questions) = {
    val (minR,maxR) = getOrderedEdge(q)
    if(q.m==0)
      return (true,q)
    if(minR==1)
      return (((maxR - q.m) >= 1), q)
    else
      return ( isAvailable(minR,maxR,q.m,0), q )
  }

  def isAvailable(minR:Int, maxR:Int, m:Int, reduceCount:Int) : Boolean = {
    if(m >= minR){
      val newM = m - minR
      val newMaxR = maxR - 1
      if(DEBUG) debug(s"minR:$minR maxR:$maxR m:$m newM:$newM newMaxR:$newMaxR")
      // minR:2 maxR:3 m:2 newM:0 newMaxR:2
      if(newM==0) {
        // remove all row
        return checkMinSquare( min(minR,newMaxR), max(minR,newMaxR), newM, reduceCount+1)
      } else {
        // Continue...
        return isAvailable( min(minR,newMaxR), max(minR,newMaxR), newM, reduceCount+1)
      }
    }else{
      return checkMinSquare(minR,maxR,m,reduceCount)
    }
  }

  def checkMinSquare(minR:Int, maxR:Int, m:Int, reduceCount:Int) : Boolean = {
    if(DEBUG) debug(s"CheckMinSquare: minR:$minR maxR:$maxR m:$m")
    // m must < minR
    if(minR==1){
      if(reduceCount==0)
        return (maxR - m) >= 1
      else
        return (maxR - m) == 1
    } else {
      if(m==0){
        return (minR+maxR-1) >= 3
      }else{
        val remainMinR = minR - {if(m>0) 1 else 0} // must not be 1
        val remainMaxR = maxR - m // must not be 1
        if(remainMinR == 1 || (remainMaxR == 1 && remainMinR-1 == 1 ) )
          return false
        else
          return ((minR+maxR-1)-m >=4 )
      }
//
//
//      val remainMinR = minR - {if(m>0) 1 else 0} // 2
//      val remainMaxR = maxR - m // 2
//      //if(!(remainMinR > 1 && remainMaxR > 1)){
//      if(m > 0 && remainMinR > 1 && remainMaxR > 1){
//        // Try fill min side
//        return ((minR+maxR-1)-m >=4 )
//      }
//      // could removed this logic
//      return (remainMinR > 1 && remainMaxR > 1)
    }
  }

  def getOrderedEdge(q:Questions) : (Int,Int) = {
    if(q.r>q.c)
      (q.c,q.r)
    else
      (q.r,q.c)
  }

  def printSquare(r:Int, c:Int, m:Int) : String = {
    //Array.tabulate[Int](minR,maxR)
    //Array.fill[String](r,c){"."}
    val square = ArrayBuffer.fill(r,c)(".")

    fillMineInSquare(square,0,0,r,c,m)

    fillIndicatorInSquare(square,r,c)

    fillClickEntry(square)

    square.map{ r =>
      if(DEBUG)
        r.mkString
      else
        r.map{_.replace('-','.')}.mkString
    }.mkString("\r\n")
  }

  def fillMineInSquare(square:ArrayBuffer[ArrayBuffer[String]],rs:Int,cs:Int,r:Int,c:Int,m:Int) {
    debug(s"rs:$rs cs:$cs r:$r c:$c m:$m")
    val isRmin = (r-rs) < (c-cs)
    val isFillFullLine = m >= {if(isRmin) r-rs else c-cs}
    if(isFillFullLine){
      // Fill full line from min row
      if(isRmin){
        // reduce column (fill row)
        debug(s"(${min(m,(r-1))},$cs)")
        for( i <- rs to min(rs+m-1,(r-1)) ) {
          square(i)(cs) = "*"
        }
        if(m > (r-rs)){
          fillMineInSquare(square,rs,cs+1,r,c,m-(r-rs))
        }
      }else{
        // reduce row (fill column)
        for( i <- cs to min(cs+m-1,(c-1)) ) {
          square(rs)(i) = "*"
        }
        if(m > (c-cs)){
          fillMineInSquare(square,rs+1,cs,r,c,m-(c-cs))
        }
      }
    }else{
//      debug(s"FULL:(${min(cs+m-1,(c-1))},$cs)")
      // Fill partial line from max row
      if(isRmin){
        // ugly handle max row too close edge issue.
        if(min(cs+m-1,(c-1)) == c-2 && rs+1 <= r-1){
          for( i <- cs to min(cs+m-1,(c-1))-1 ) {
            square(rs)(i) = "*"
          }
          square(rs+1)(cs) = "*"
        }else{
          for( i <- cs to min(cs+m-1,(c-1)) ) {
            square(rs)(i) = "*"
          }
        }
      } else {
        // ugly handle max row too close edge issue.
        if(min(rs+m-1,(r-1)) == r-2 && cs+1 <= c-1){
          for( i <- rs to min(rs+m-1,(r-1))-1 ) {
            square(i)(cs) = "*"
          }
          square(rs)(cs+1) = "*"
        }else{
          for( i <- rs to min(rs+m-1,(r-1)) ) {
            square(i)(cs) = "*"
          }
        }
      }

    }
  }

  def fillIndicatorInSquare(square:ArrayBuffer[ArrayBuffer[String]],r:Int,c:Int) {
    for(i <- 0 to square.length-1){
      for(j <- 0 to square(i).length-1){
        if(square(i)(j)!="*" && checkNeighborHasMine(square,i,j))
          square(i)(j)="-"
      }
    }

  }

  def checkNeighborHasMine(square:ArrayBuffer[ArrayBuffer[String]],r:Int,c:Int) : Boolean = {
    for(i <- max(r-1,0) to min(r+1,square.length-1)){
//      debug(s"r:$r, c:$c, i:"+i)
      for(j <- max(c-1,0) to min(c+1,square(i).length-1)){
        if( !( i==r && j==c ) && square(i)(j)=="*"){
          return true
        }
      }
    }
    return false
  }

  def fillClickEntry(square:ArrayBuffer[ArrayBuffer[String]]) {
    for(i <- 0 to square.length-1){
      for(j <- 0 to square(i).length-1){
        if(square(i)(j)=="."){
          square(i)(j)="c"
          return
        }
      }
    }
    // not found find "-"
    for(i <- 0 to square.length-1){
      for(j <- 0 to square(i).length-1){
        if(square(i)(j)=="-"){
          square(i)(j)="c"
          return
        }
      }
    }
  }

}
