package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Optionals.Optional
import util.Sequences.*

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var minesSeq: Sequence[(Int, Int)] =
    val r = Random()
    var s: Sequence[(Int, Int)] = Sequence.empty
    var len = s.length
    while (len < mines)
      s = (s concat Sequence.generate(() => (r.nextInt(size), r.nextInt(size)), mines - len)).uniqueSequence
      len = s.length
    s

  private var hit: Boolean = false
  private var discovered: Sequence[(Int, Int)] = Sequence.empty

  private def dist(p1: (Int, Int), p2: (Int, Int)): (Int, Int) =
    (Math.abs(p1._1 - p2._1), Math.abs(p1._2 - p2._2))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val p = (x, y)
    val isMine = minesSeq.contains(p)
    if isMine then
      hit = true
      OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
    else
      discovered = discovered.append(p)
      val adjacentMines = minesSeq.filter(m => { val d = dist(p, m); d._1 <= 1 && d._2 <= 1 }).length
      OptionToOptional(ScalaOptional.Just(adjacentMines))

  def won: Boolean = (!hit) && discovered.length == size * size - mines