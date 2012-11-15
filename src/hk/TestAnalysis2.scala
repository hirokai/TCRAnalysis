import ij.process.ImageProcessor
import ij.ImagePlus
import ij.gui.ImageWindow
import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io.PrintWriter


object TestAnalysis2 {
	type Matrix = Array[Array[Float]]
	var param: Array[Float] = _
  
	var width, height: Int = _
	var operations: Array[ImageProcessor=>ImageProcessor] = _
	var images: ArrayBuffer[ImageProcessor] = new ArrayBuffer[ImageProcessor]

	def run(_ip: ImageProcessor, paramStr: String): ImageProcessor = {
  	  val ip = _ip
      val _ = new ImageWindow(new ImagePlus("Cropped",ip))
      width = ip.getWidth
  	  height = ip.getHeight
  	  param = paramStr.split(",").map(_.toFloat).toArray
  	  val p: Array[Float] = new Array[Float](width*height)
  	  graddir(p,ip)
  	  val pw = new PrintWriter("debugout.txt")
  	  pw.println(p.sorted.reverse.mkString(" "))
  	  pw.close()
  	  val result = corr(p,param(0).toInt)
  	  println(result.mkString(" "))
      ip
	}
	def graddir(p: Array[Float], ip: ImageProcessor) {
	  for(x <- 1 until width-1; y <- 1 until height-1){
		  val dx = (ip.get(x+1,y).toFloat-ip.get(x-1,y))/2
		  val dy = (ip.get(x,y+1).toFloat-ip.get(x,y-1))/2
		  p(x*height+y) = atan2(dy,dx).toFloat
//		  if(p(x*height+y)==0)
//		    println(x,y,ip.get(x,y),dx,dy)
	  }
//	  println(p.sum/p.length)
	}
	def corr(p: Array[Float],max: Int): Array[Float] = {
		var r = new Array[Float](max+1)
		var count = new Array[Float](max+1)
	  for(x <- 0 until width; y <- 0 until height){
		  val vs = (
		      cos(p(x*height+y)).toFloat,
		      sin(p(x*height+y)).toFloat
		      )
		  for(i <- -max to max; j <- -max to max;
			if x+i>=0 && x+i<width && y+j>0 && y+j<height){
			  val v = (
					  cos(p((x+i)*height+(y+j))).toFloat,
					  sin(p((x+i)*height+(y+j))).toFloat
					  )
			  val dist = round(sqrt(i*i+j*j)).toInt
			  if(dist<=max){
				  r(dist) = r(dist) + vs._1*v._1 + vs._2 * v._2
				  count(dist) += 1
			  }
		  }
	  }
	r = r.indices.map(i => r(i)/count(i)).toArray
	r
	}
}