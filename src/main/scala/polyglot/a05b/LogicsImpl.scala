package polyglot.a05b

import polyglot.Pair
import polyglot.a05b.Logics
import scala.util.Random
import scala.math.abs

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics {
  private var random = scala.util.Random(42)
  private var firstHit = Pair(this.random.nextInt(this.size),this.random.nextInt(this.size))
  private var count = 0

  override def tick(): Unit = this.count = this.count + 1

  override def isOver: Boolean =  this.firstHit.getX - this.count < 0 |
                                  this.firstHit.getX + this.count >= this.size |
                                  this.firstHit.getY - this.count < 0 |
                                  this.firstHit.getY + this.count >= this.size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == this.firstHit.getX & abs(y - this.firstHit.getY) <= this.count) |
    (y == this.firstHit.getY & abs(x - this.firstHit.getX) <= this.count) |
    (abs(x - this.firstHit.getX) == abs(y - this.firstHit.getY) & abs(y - this.firstHit.getY) <= this.count)
}