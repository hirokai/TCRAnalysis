package hk;

import ij.process.ImageProcessor
import scala.collection.mutable.ArrayBuffer
import scala.math._


object TestAnalysis3 {
	type Matrix = Array[Array[Float]]
	var param: Array[Float] = _
  
	var width, height: Int = _
	var operations: Array[ImageProcessor=>ImageProcessor] = _
	var images: ArrayBuffer[ImageProcessor] = new ArrayBuffer[ImageProcessor]

	def run(_ip: ImageProcessor, paramStr: String): Float = {
  	  var ip = _ip
      width = ip.getWidth
  	  height = ip.getHeight
  	  param = paramStr.split(",").map(_.toFloat).toArray
  	  val p: Array[Float] = new Array[Float](width*height)
  	  val sum = (for(x <- 0 until width; y <- 0 until height) yield ip.get(x,y)).toArray.sum
  	  val entropy = (for(x <- 0 until width; y <- 0 until height) yield {
  	    val p = ip.get(x,y).toFloat/sum
  	   if(p>0) -p * log(p) else 0
  	  }).toArray.sum.toFloat
//  	  println(entropy)
//      val f = new ImageWindow(new ImagePlus("Entropy: "+entropy.toString,ip))

 // 	  val pw = new PrintWriter("debugout.txt")
 // 	  pw.close
      entropy
	}
}