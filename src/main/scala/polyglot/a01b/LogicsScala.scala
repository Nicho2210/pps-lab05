package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences.*
import util.Sequences.Sequence.*
import scala.util.Random

import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */

trait LogicsScala:
  def hit(x: Int, y: Int): java.util.Optional[Integer]

  def won: Boolean

object LogicsScala:
  def apply(size: Int, mines: Int): LogicsScala = LogicsImpl(size, mines)

  private class LogicsImpl(private val size: Int, private val mines: Int) extends LogicsScala:

    private var minesPos = Sequence.Nil[(Int, Int)]()
    private var countMines = 0
    private var countClicks = 0
    while (countMines < mines)
      val pos = (Random.nextInt(size), Random.nextInt(size))
      if (!minesPos.contains(pos)) then
        minesPos = minesPos.concat(Sequence.Cons(pos, Sequence.Nil()))
        countMines = countMines + 1
        println(s"Pos: (${pos._1}, ${pos._2})")


    override def hit(x: Int, y: Int): java.util.Optional[Integer] =
      OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter

    override def won = false
