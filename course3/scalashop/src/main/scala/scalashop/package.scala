
import common._

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

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    ???
  }

}

object main extends App{
  def rgbaArr(rgba: RGBA): List[Int] = {
    List(
      scalashop.red(rgba),
      scalashop.green(rgba),
      scalashop.blue(rgba),
      scalashop.alpha(rgba)
    )
  }

  def printImg(img: Img): Unit ={
    for {
      x <- 0 until w
      y <- 0 until h
      s = if(y == h-1) "\n" else " "
    } print(rgbaArr(img.apply(x, y)) + s)
//    } print(x + ":" + y + "\n")
    println()
  }

  println("START")
  val w:Int = 3
  val h:Int = 3
  val img = new Img(w,h)
  for {
    x <- 0 until w
    y <- 0 until h
    v = 32 * (x + 1) + y
    c = scalashop.rgba(v, v, v, v)
  } img.update(x, y, c)
  printImg(img)
//  println(rgbaArr(0x10101010))
  println("FINISH")
}
