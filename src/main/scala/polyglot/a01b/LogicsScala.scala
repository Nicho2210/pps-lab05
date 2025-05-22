package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.*
import util.Sequences.Sequence.*
import scala.util.Random

import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */

trait LogicsScala:
  def size: Int
  def mines: Int
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

object LogicsScala:
  def apply(size: Int, mines: Int): LogicsScala = LogicsImpl(size, mines)

  private class LogicsImpl(override val size: Int, override val mines: Int) extends LogicsScala:

    private var _minesPos = Sequence.Nil[(Int, Int)]()
    private var _countMines = 0
    private var _countClicks = 0

    while _countMines < mines do
      val pos = (Random.nextInt(size), Random.nextInt(size))
      if !_minesPos.contains(pos) then
        _minesPos = _minesPos.concat(Sequence.Cons(pos, Sequence.Nil()))
        _countMines = _countMines + 1
        println(s"Pos: (${pos._1}, ${pos._2})")


    override def hit(x: Int, y: Int): java.util.Optional[Integer] =
      if _minesPos.contains((x, y)) then
        OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
      else
        _countClicks = _countClicks + 1
        var countNeighbors = 0
        for
          i <- x - 1 to x +1
          j <- y - 1 to y + 1
        yield countNeighbors = countNeighbors + (if _minesPos.contains((i, j)) then 1 else 0)
        OptionToOptional(ScalaOptional.Just(countNeighbors))


    override def won: Boolean = size * size - mines == _countClicks
