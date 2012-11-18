import ij.process.ImageProcessor
import ij.ImagePlus
import ij.gui.ImageWindow
import scala.collection.mutable.ArrayBuffer
import scala.math._
import ij.plugin.filter.BackgroundSubtracter
import java.awt.Color


object TestAnalysis {
	type Matrix = Array[Array[Float]]
	var param: Array[Float] = _
  
	var width, height: Int = _
	var operations: Array[ImageProcessor=>ImageProcessor] = _
	var images: ArrayBuffer[ImageProcessor] = new ArrayBuffer[ImageProcessor]

	def run(_ip: ImageProcessor, paramStr: String): ImageProcessor = {
  	  var ip = _ip
  	  param = paramStr.split(",").map(_.toFloat).toArray
  	  
  	  operations = Array(multilaplacian(param(0).toInt,param(1).toInt))
      
  	  val f = new ImageWindow(new ImagePlus("Cropped",ip))
  	  f.setLocation(400,200)
      f.setVisible(true)
  	  width = ip.getWidth; height = ip.getHeight
  	  
  	  for(i <- operations.indices){
  	    ip = operations(i)(ip)
  	    val imp = new ImagePlus("Op #%d".format(i),ip)
  	    val frame = new ImageWindow(imp)
  	    frame.setLocation(400+(width+20)*(i+1),200)
  	    frame.setVisible(true) 
  	  }
      ip
	}
	
	def threshold(in: ImageProcessor): ImageProcessor = {
	  val ip = in.duplicate
	  ip.autoThreshold()
	  ip.invert()
	  ip
	}

	def subbg(v: Int)(in: ImageProcessor): ImageProcessor = {
	  val ip = in.duplicate
	  // val sub = (new BackgroundSubtracter).rollingBallBackground(ip,v,false,false,false,false,false)
	  ip
	}
	
	def multilaplacian(minsize: Int, maxsize:Int)(in: ImageProcessor): ImageProcessor = {
	  val pixels = new ArrayBuffer[Array[Int]]
	  for(size <- minsize to maxsize; if size%2==1){
		  val gau = gaussian(size)(in)
//		  new ImageWindow(new ImagePlus("Processed",gau))
	      pixels += laplacian(3)(gau).map(_*size)
	  }
      val result = findLocalMax(pixels.toArray,minsize,maxsize,param(2).toInt)
      println(result.length)
      val ips = new ArrayBuffer[ImageProcessor](pixels.length)
	  for(plane <- pixels){
	    val p = in.duplicate.convertToRGB
	    normalize(plane,p)
	    ips.append(p)
	  }
      val disp = in.duplicate.convertToRGB
      disp.setColor(Color.red)
      for(r <- result){
        val x = r._1; val y = r._2; val z = r._3
        ips(z).putPixel(x,y,Array[Int](255,0,0))
//        val rad = sqrt(2*(minsize+z*2)).toInt
        val rad = minsize+z*2
        disp.drawOval(x-rad,y-rad,rad*2,rad*2)
        println(x,y,z,pixels(z)(x*height+y),ips(z).get(x,y))
      }	  
	  ips.foreach(im => {
	    new ImageWindow(new ImagePlus("Processed",im))
	  })
	  disp
	}
	
	def findLocalMax(ips: Array[Array[Int]],minsize:Int,maxsize:Int,threshold:Int): Array[(Int,Int,Int)] = {
	  val crop = (maxsize+1)/2+1
//	  val crop = 1
	  // def tosize(i:Int):Int = minsize+i*2
	  def eget(x:Int,y:Int,z:Int): Int = {
	    ips(z)(x*height+y)
	  }
	  def cget(x:Int,y:Int,z:Int,w:Int): Array[Int] = {
	    (for(i<- -w to w; j <- -w to w; k <- -w to w; if i!=0 || j!=0 || k!=0 &&
	    x+i>=0 && x+i<width && y+j>=0 && y+j<height && z+k>=0 && z+k<(maxsize-minsize)/2)
	      yield eget(x+i,y+j,z+k)).toArray
	  }
	  val maxs = new ArrayBuffer[(Int,Int,Int)]
	  val cgetw = param(4).toInt
	  for(z <- cgetw until ips.length-cgetw; x <- crop until width-crop; y <- crop until height-crop){
	    val v = eget(x,y,z)
	    if(v >= threshold && v >= cget(x,y,z,cgetw).max+param(3))
	      maxs.append((x,y,z))
	  }
	  maxs.toArray
	}
		  def normalize(p: Array[Int],ip:ImageProcessor) {
	      val wid = 1
		  val maxval: Float = p.max + 1
          // val minval: Float = p.min + 1
          val range: Float = (maxval-0).toFloat
       // 	  println(maxval,minval)
          for(x <- wid until width-wid; y <- wid until height-wid){
             ip.set(x,y,max(0,((p(x*height+y)/range*4000).toInt)))
           }
	       ip
	  }
	def laplacianimg(size: Int)(in: ImageProcessor): ImageProcessor = {

	  val p = laplacian(size)(in)
	  val ip = in.duplicate
	  normalize(p,ip)
	  ip
	}
	def laplacian(size: Int)(in: ImageProcessor): Array[Int] = {

	  if(size <= 0 || size%2==0)throw new IllegalArgumentException
	  val wid = (size-1)/2
	  val mat = laplacianMat(size,size.toFloat/2)
//	  printMat(mat)
	  val ip = in.duplicate
	  val p = new Array[Int](width*height)
      for(x <- wid until width-wid; y <- wid until height-wid){
        p(x*height+y) = appmat(ip,x,y,mat,size).toInt
      }
	  p
	}


	def diff(in: ImageProcessor): ImageProcessor = {
	  val ip = in.duplicate
	 //  val g_th = pow(((in.maxValue - in.minValue)/10000),2)
	  for(x<-1 until width-1; y<-1 until height-1){
	    val dx = (in.get(x+1,y).toFloat-in.get(x-1,y))/2
	    val dy = (in.get(x,y-1).toFloat-in.get(x+1,y-1))/2
	    ip.set(x,y,(sqrt(dx*dx+dy*dy)*100).toInt)
	  }
	  ip
	}

	def findMax(in: ImageProcessor): ImageProcessor = {
	  val ip = in.duplicate
	  val dx: Array[Float] = new Array[Float](width*height)
	  val dy: Array[Float] = new Array[Float](width*height)
	  val g_th = pow(((in.maxValue - in.minValue)/10000),2)
	  for(x<-1 until width-1; y<-1 until height-1){
	    dx(x*height+y) = (ip.get(x+1,y).toFloat-ip.get(x-1,y))/2
	    dy(x*height+y) = (ip.get(x,y+1).toFloat-ip.get(x,y-1))/2
	  }
	  val dx2: Array[Float] = new Array[Float](width*height)
	  val dy2: Array[Float] = new Array[Float](width*height)
	  for(x<-1 until width-1; y<-1 until height-1){
	    dx2(x*height+y) = (dx((x+1)*height+y).toFloat-dx((x-1)*height+y))/2
	    dy2(x*height+y) = (dx(x*height+(y+1)).toFloat-dy(x*height+(y-1)))/2
	  }
//	  var p: Array[Float] = new Array[Float](width*height)
//	  val max = p.max
//	  p = p.map(_/max*65536)
	  for(x<-0 until width; y<-0 until height){
	    val dsq = dx(x*height+y)*dx(x*height+y)+dy(x*height+y)*dy(x*height+y)
//	    val d2sq = dx2(x*height+y)*dx2(x*height+y)+dy2(x*height+y)*dy2(x*height+y)
	    if(dsq<g_th && dx2(x*height+y)<0 && dy2(x*height+y)<0)
	      ip.set(x,y,65535)
	    else
	      ip.set(x,y,0)
	  }
	  ip.setMinAndMax(0,65535)
	  ip	  
	}
	def subgauss(size: Int)(in: ImageProcessor): ImageProcessor = {
	  if(size <= 0 || size%2==0)throw new IllegalArgumentException
	  val wid = (size-1)/2
	  val mat = gaussianMat(size,size.toFloat/2)
	  val ip = in.duplicate
	  val res: Matrix = (1 to width).map(a=>new Array[Float](height)).toArray
      for(x <- wid until width-wid; y <- wid until height-wid){
        res(x)(y) = appmat(ip,x,y,mat,size)
      }
      val offset: Float = (for(x <- wid until width-wid; y <- wid until height-wid) yield
    	  res(x)(y) - in.get(x,y).toFloat).max
 	  println(offset)
      for(x <- wid until width-wid; y <- wid until height-wid){
    	  ip.set(x,y,(in.get(x,y).toFloat - res(x)(y) + offset).toInt)
      }
	  ip
	}
	def gaussian(t: Int)(in: ImageProcessor): ImageProcessor = {
	  val size = 21
	  if(size <= 0 || size%2==0)throw new IllegalArgumentException
	  val wid = (size-1)/2
	  val mat = gaussianMat(size,t)
	  val ip = in.duplicate
      for(x <- wid until width-wid; y <- wid until height-wid){
        val p = appmat(ip,x,y,mat,size).toInt
        ip.set(x,y,p)
      }
	  ip
	  }

	def appmat(im: ImageProcessor, x: Int, y: Int, mat: Matrix, dim: Int): Float = {
	  val wid = (dim-1)/2
	  var v = 0f
	  for(i <- 0 until dim; j <- 0 until dim){
		  v += im.get(x+i-wid,y+j-wid)*mat(i)(j)
	  }
	  v
	}
	
	def printMat(m: Matrix) {
	  val dimX = m.length
	  // val dimY = metricsFile(0).length
	  for(x<-0 until dimX){
	    println(m(x).map(_.toString).mkString(" "))
	  }
	}

	def gaussianMat(dim: Int, t: Float): Array[Array[Float]] = {
		if(dim%2==0)
			throw new Exception("dim must be odd.")
		val arr = Array.fromFunction(i=>{
			Array.fromFunction({j=>
				val x = (i - (dim-1)/2).toFloat
				val y = (j - (dim-1)/2).toFloat
				(1.0/3.141592/2/t*exp((0f-x*x-y*y)/2/t)).toFloat
			})(dim)})(dim)
		val ret = arr.map(a=>a.map(b=>b/sum(arr)))
		ret
	}

	def laplacianMat(dim: Int, sigma: Float): Array[Array[Float]] = {
		if(dim!=3)
			throw new Exception("Only 3x3 laplacian is supported so far")
		val mat3 = Array(Array(-1f,-1f,-1f),Array(-1f,8f,-1f),Array(-1f,-1f,-1f))
		return mat3

		if(dim%2==0)
			throw new Exception("dim must be odd.")
		val arr = Array.fromFunction(i=>{
			Array.fromFunction({j=>
				val x = (i - (dim-1)/2).toFloat
				val y = (j - (dim-1)/2).toFloat
				(1-(x*x+y*y)/2/(sigma*sigma)).toFloat
			})(dim)})(dim)
		val ret = arr.map(a=>a.map(b=>b/sum(arr)))
		printMat(ret)
		ret
	}
	
	
	def sum(arr: Array[Array[Float]]): Float = {
		arr.map(row=>row.sum).sum
	}
}