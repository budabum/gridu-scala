package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def checkPos(pos: Pos): Boolean ={
      val r = pos.row
      val c = pos.col
      try{
        levelVector(r)(c) != '-'
      }
      catch{
        case e: IndexOutOfBoundsException => false
      }
    }

    checkPos
  }

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val poses = for{
      ri <- levelVector.indices
      ci <- levelVector(ri).indices
      if levelVector(ri)(ci) == c
    } yield Pos(ri, ci)
    poses(0)
  }

  //private
  lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}

class Aaa extends StringParserTerrain {
  /**
    * A ASCII representation of the terrain. This field should remain
    * abstract here.
    */
  override val level =
    """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

  val pos1 = Pos(1,1)
  val pos2 = Pos(2,2)
  val pos3 = Pos(12,7)
}

object SPTMain extends App{
  println("START")
  val aaa = new Aaa
  println(aaa.vector)
  println(aaa.terrainFunction(aaa.vector)(aaa.pos1))
  println(aaa.terrainFunction(aaa.vector)(aaa.pos2))
  println(aaa.terrainFunction(aaa.vector)(aaa.pos3))
  println("END")
}
