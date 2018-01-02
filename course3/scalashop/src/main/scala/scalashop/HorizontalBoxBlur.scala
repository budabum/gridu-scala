package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scala.collection.mutable.ListBuffer

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      y <- from until end
      x <- 0 until src.width
    } dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  def parallel2(tasks: Seq[ForkJoinTask[Unit]]): Unit = {
    if(tasks.isEmpty) return
    if(tasks.size > 1){
      def t0 = tasks.head
      def t1 = tasks.tail.head
      parallel(t0, t1)
      parallel2(tasks.tail.tail)
    }
    else{
      def t0 = tasks.head
      t0.invoke()
    }
  }
  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur2(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val lb: ListBuffer[(Int, Int)] = ListBuffer()
    val h = src.height
    val n = numTasks
    for {
      y <- 0 to h by n
      if y <= h
    } lb += ((y, if(y+n <= h) y+n else h))

    val tasks: Seq[ForkJoinTask[Unit]] = lb.map(n => task {blur(src, dst, n._1, n._2, radius)})
    parallel2(tasks)
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val lb: ListBuffer[(Int, Int)] = ListBuffer()
    val h = src.height
    val n = numTasks
    for {
      y <- 0 to h by n
      if y <= h
    } lb += ((y, if(y+n <= h) y+n else h))

    lb.toVector.par.foreach(n => blur(src, dst, n._1, n._2, radius))
  }

}
