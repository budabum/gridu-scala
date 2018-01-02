
import common._

import scala.collection.mutable.ListBuffer
import scalashop.{Img, RGBA}

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def rgbaArr(rgba: RGBA): Array[Int] = {
    Array(
      scalashop.red(rgba),
      scalashop.green(rgba),
      scalashop.blue(rgba),
      scalashop.alpha(rgba)
    )
  }

  def fillImg(w: Int, h: Int): Img = {
    val img = new Img(w,h)
    for {
      x <- 0 until w
      y <- 0 until h
      v = y * w + x
      c = rgba(v, v, v, v)
    } img.update(x, y, c)
    img
  }

  def printImg(img: Img): Unit ={
    for {
      y <- 0 until img.height
      x <- 0 until img.width
      s = if(x == img.width-1) "\n" else " "
    } print(rgbaArr(img.apply(x, y)).toList + s)
    //    } print(x + ":" + y + "\n")
    println()
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val neighbors: ListBuffer[Array[Int]] = ListBuffer()
    for {
      y1 <- y-radius to y+radius
      x1 <- x-radius to x+radius
      if (0 <= x1 && x1 < src.width) && (0 <= y1 && y1 < src.height) //&& !(x1 == x && y1 == y)
    } neighbors += rgbaArr(src.apply(x1, y1))
//    println(neighbors.map(n=>n.toList))
    val rgbaMedian = neighbors.fold(Array(0,0,0,0))((n, a) =>
      a.zip(n).map{case (i1, i2) => i1 + i2}).map(n => n / neighbors.size)
    val rgbaMedianTuple = rgbaMedian match {case Array(a,b,c,d) => (a,b,c,d)}
    rgba _ tupled rgbaMedianTuple
  }

}

object main extends App{
  import scalashop._

  println("START")
  val img = fillImg(3,3)
  printImg(img)
  println(rgbaArr(boxBlurKernel(img, 1, 2, 1)).toList)
//  println(rgbaArr(0x10101010))
  println("FINISH")
}
