package polyglot.a01a

import scala.util.Random
/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */

enum ResultScala:
  case Hit, Miss, Won, Lost
  
object ResultScalaUtils:
  def name(result: ResultScala): String = result match
    case ResultScala.Hit => "Hit"
    case ResultScala.Miss => "Miss"
    case ResultScala.Won => "Won"
    case ResultScala.Lost => "Lost"

trait LogicsScala:
  def hit(row: Int, col: Int): ResultScala

object LogicsScala:
  def apply(size: Int, boat: Int): LogicsScala = LogicsImpl(size, boat)

  private class LogicsImpl(private val size: Int, private val boat: Int) extends LogicsScala:
    
    private val startingBoat = (Random.nextInt(size - boat), Random.nextInt(size - boat))
    println(s"Boat => (${startingBoat._1}, ${startingBoat._2}")
    private var countHit = 0
    private var countMiss = 0
    private val maxMiss = 5

    override def hit(row: Int, col: Int): ResultScala =
      if row == startingBoat._1 && col >= startingBoat._2 && col < startingBoat._2 + boat then 
        countHit = countHit + 1
        if countHit == boat then 
          ResultScala.Won 
        else ResultScala.Hit 
      else
        countMiss = countMiss + 1
        if countMiss == maxMiss then 
          ResultScala.Lost 
        else ResultScala.Miss