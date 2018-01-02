package scalashop

import org.scalameter._
import common._

import scala.collection.mutable.ListBuffer

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      x <- from until end
      y <- 0 until src.height
    } dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val lb: ListBuffer[(Int, Int)] = ListBuffer()
    val w = src.width
    val n = numTasks
    for {
      x <- 0 to w by n
      if x <= w
    } lb += ((x, if(x+n <= w) x+n else w))

    lb.toVector.par.foreach(n => blur(src, dst, n._1, n._2, radius))
  }

}

object main extends App {
  import VerticalBoxBlur._

  val w = 1000
  val h = 1000
  val src = fillImg(w, h)
  val dst1 = new Img(w, h)
  val dst2 = new Img(w, h)

  blur(src, dst1, 0, w, 1)
  parBlur(src, dst2, 3, 1)

  printImg(src)
  printImg(dst1)
  printImg(dst2)
}
