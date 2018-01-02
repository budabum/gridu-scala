package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ForkJoinTasks

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

  def parallel2(tasks: Seq[ForkJoinTask[Unit]]): Unit = {
    if(tasks.isEmpty) return
    if(tasks.size > 1){
      def t0 = tasks.head
      parallel(t0, task {parallel2(tasks.tail)})
    }
    else{
      tasks.head.invoke()
    }
  }
  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur2(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val lb: ListBuffer[(Int, Int)] = ListBuffer()
    val w = src.width
    val n = numTasks
    for {
      x <- 0 to w by n
      if x <= w
    } lb += ((x, if(x+n <= w) x+n else w))

    val tasks: Seq[ForkJoinTask[Unit]] = lb.map(n => task {blur(src, dst, n._1, n._2, radius)}).toSeq
    parallel2(tasks)
  }

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

  def parBlur1(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val lb: ListBuffer[(Int, Int)] = ListBuffer()
    val w = src.width
    val n = w / numTasks
    for {
      x <- 0 until w by n
      if x <= w
    } lb += ((x, if(x+n <= w) x+n else w))

    println(lb)
    println(lb.size)

    lb.toVector.par.foreach(n => task{blur(src, dst, n._1, n._2, radius)}.invoke())
  }

}

object main extends App {
  import VerticalBoxBlur._

  val w = 8
  val h = 8
  val src = fillImg(w, h)
  val dst1 = new Img(w, h)
  val dst2 = new Img(w, h)

  blur(src, dst1, 0, w, 2)
  parBlur(src, dst2, 4, 2)

  printImg(src)
  printImg(dst1)
  printImg(dst2)
}
