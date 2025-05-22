package polyglot.a05b

import polyglot.a05b.Logics
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val startingPoint = (1 + Random.nextInt(size), 1 + Random.nextInt(size))
  private var count = 0

  override def tick(): Unit =
    count = count + 1

  override def isOver: Boolean =
    startingPoint._1 - count < 0 ||
    startingPoint._1 + count >= size ||
    startingPoint._2 - count < 0 ||
    startingPoint._2 + count >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == startingPoint._1 && isBetween(y)(startingPoint._2 - count, startingPoint._2 + count)) ||
    (y == startingPoint._2 && isBetween(x)(startingPoint._1 - count, startingPoint._1 + count)) ||
    (math.abs(x - startingPoint._1) <= count &&
     math.abs(y - startingPoint._2) <= count &&
     math.abs(x - startingPoint._1) == math.abs(y - startingPoint._2))

  private def isBetween(n: Int)(min: Int, max: Int): Boolean = n >= min && n <= max