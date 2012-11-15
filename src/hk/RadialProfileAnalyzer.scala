package hk

import scala.math._
import java.awt.image.BufferedImage
import ij.process.ImageProcessor
import TcrAlgorithm._

/**
 * ToDo: ImageProcessor holds value as Float, so conversion between double and float is cumbersome.
 */
object RadialProfileAnalyzer{
	def checkRadius(bf:Circle, tcr:Circle,width:Int,height:Int): Boolean = {
		val xmin = min(bf.cx-bf.r,tcr.cx-tcr.r)
		val xmax = max(bf.cx+bf.r,tcr.cx+tcr.r)
		val ymin = min(bf.cy-bf.r,tcr.cy-tcr.r)
		val ymax = max(bf.cy+bf.r,tcr.cy+tcr.r)
		xmin>=0 && xmax < width && ymin >= 0 && ymax < height
	}
	def availableMetrics: Map[String,String] = {
		Map("minIntensity"->"Minimum intensity","pixelSum"->"Total intensity","entropy"->"Entropy","radcom"->"CoM","radskew"->"Skewness",
			"entropyPerPixel"->"Entropy per pixel", "distMean"->"Mean distance", "distVariance"->"Distance variance",
			"pixelSumBS"->"Total intensity (BG subtracted)","entropyBS"->"Entropy (BG subtracted)","radcomBS"->"CoM (BG subtracted)",
			"radskewBS"->"Skewness (BG subtracted)","entropyPerPixelBS"->"Entropy per pixel (BG subtracted)",
			"distMeanBS"->"Mean distance (BG subtracted)", "distVarianceBS"->"Distance variance (BG subtracted)",
			"radgini"->"Gini coefficient", "radginiBS"->"Gini coefficient (BG subtracted)")
	}

	def getRadialProfile(ip: ImageProcessor, bf: Circle, tcr: Circle, num_bins: Int = -1): Option[Array[Double]]  = {
		val height = ip.getHeight
		val width = ip.getWidth
		def dist(ax:Double,ay:Double,bx:Double,by:Double) = sqrt(pow(ax-bx,2)+pow(ay-by,2))

		val numBin = if(num_bins>0) num_bins else Config.numBin
		//var index = 0
		//val intensity = new Array[Double](numBin)
		if(!checkRadius(bf,tcr,width,height))
			return None
		val count = new Array[Int](numBin)
		val sum = new Array[Int](numBin)
		val rint = tcr.r / numBin
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r)){
			val bin = (floor(dist(tcr.cx,tcr.cy,x,y)/rint)).toInt
			if(bin<numBin){
				count(bin) +=  1
				sum(bin) += ip.get(x.toInt,y.toInt).toInt
			}
		}
		val ret = sum.zip(count).map(a=>a._1.toDouble/a._2)
		Some(ret)
	}
	def calcTcrCenter(ip: ImageProcessor, bf: Circle, algo: TcrAlgorithm):Point2f = {
		calcTcrCenter(ip,new Point2f(bf.cx,bf.cy),bf.r,algo)
	}
	def calcTcrCenter(ip: ImageProcessor, bf_center: Point2f, radius: Double, algo: TcrAlgorithm):Point2f = {
		val power = algo match {
			case TcrAlgorithm.CoM | TcrAlgorithm.CoMsubtractBG => 1
			case TcrAlgorithm.CoM3 | TcrAlgorithm.CoM3subtractBG => 3
		}
		val cx = bf_center.x; val cy = bf_center.y; val r = radius
		val h = ip.getHeight; val w = ip.getWidth
		val xmin = max(round(cx-r),0).toInt; val xmax = min(round(cx+r),w-1).toInt
		val ymin = max(round(cy-r),0).toInt; val ymax = min(round(cy+r),h-1).toInt
		val bg: Float = algo match {
			case TcrAlgorithm.CoM | TcrAlgorithm.CoM3 => 0
			case TcrAlgorithm.CoMsubtractBG | TcrAlgorithm.CoM3subtractBG =>
				(for(x <- xmin to xmax; y <- ymin to ymax
				     if pow(cx-x,2) + pow(cy-y,2) <= pow(r,2))
				yield ip.get(x,y)).min
		}
		var xsum = 0f; var ysum = 0f; var pixelsum = 0f
		for(x <- xmin to xmax; y <- ymin to ymax
		    if pow(cx-x,2) + pow(cy-y,2) <= pow(r,2)){
			xsum += x * pow(ip.get(x,y)-bg,power).toFloat; ysum += y * pow(ip.get(x,y)-bg,power).toFloat
			pixelsum += pow(ip.get(x,y)-bg,power).toFloat	}
		new Point2f(xsum/pixelsum,ysum/pixelsum)
	}
	def getTcrMetrics(ip: ImageProcessor, bf: Circle, tcr: Circle, radial: Option[Array[Double]]): Map[String,Float]  = {
		val radius:Float = tcr.r
		val area:Float = (radius*radius*3.14159).toFloat
		def dist(ax:Float,ay:Float,bx:Float,by:Float): Float = sqrt(pow(ax-bx,2)+pow(ay-by,2)).toFloat
		if(!checkRadius(bf,tcr,ip.getWidth,ip.getHeight))
			return Map("minIntensity"->Float.NaN,"pixelSum"->Float.NaN,"entropy"->Float.NaN,"radcom"->Float.NaN,"radskew"->Float.NaN,
				"entropyPerPixel"->Float.NaN, "distMean"->Float.NaN, "distVariance"->Float.NaN,
				"pixelSumBS"->Float.NaN,"entropyBS"->Float.NaN,"radcomBS"->Float.NaN,"radskewBS"->Float.NaN,
				"entropyPerPixelBS"->Float.NaN, "distMeanBS"->Float.NaN, "distVarianceBS"->Float.NaN,
				"radgini"->Float.NaN, "radginiBS"->Float.NaN
			)
		var minint: Int = ip.get(round(tcr.cx).toInt,round(tcr.cy).toInt)
		var pixelsum = 0f
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p = ip.get(x.toInt,y.toInt).toInt
			pixelsum += p
			minint = min(minint,p)
		}
		val pixelsumbs = pixelsum - minint*area

		//Metrics based on radial profile
		val radcom: Float = radial match {
			case Some(r) => centerofmass(r).toFloat
			case None => Float.NaN
		}
		val radcombs: Float = radial match {
			case Some(r) => centerofmass(r.map(v => {(v - minint)})).toFloat
			case None => Float.NaN
		}
		val radskew = skewness(radial).toFloat
		val radskewbs: Float = radial match {
			case Some(r) => skewness(r.map(_-minint)).toFloat
			case None => Float.NaN
		}
		val radvar = variance(radial).toFloat
		val radvarbs: Float = radial match {
			case Some(r) => variance(r.map(_-minint)).toFloat
			case None => Float.NaN
		}
		val radgini = gini(radial).toFloat
		val radginibs: Float = radial match {
			case Some(r) => gini(r.map(_-minint)).toFloat
			case None => Float.NaN
		}

		//Metrics that are directly calculated from image
		var entropy = 0f
		var entropybs = 0f
		var distmean = 0f
		var distmeanbs = 0f
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p = ip.get(x,y)
			val pb = p - minint
			val pn = p / pixelsum
			val pbn = pb / pixelsumbs
			entropy += (if(pn>0) -pn * log(pn) else 0).toFloat
			entropybs += (if(pbn>0) -pbn * log(pbn) else 0).toFloat
			distmean += p*dist(x,y,tcr.cx,tcr.cy).toFloat
			distmeanbs += pb*dist(x,y,tcr.cx,tcr.cy).toFloat
		}
		distmean /= (radius * pixelsum)
		distmeanbs /= (radius * pixelsumbs)

		var distvar = 0f
		var distvarbs = 0f
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p = ip.get(x,y)
			val pb = p - minint
			distvar += p*pow((dist(x,y,tcr.cx,tcr.cy)/radius-distmean),2).toFloat
			distvarbs += pb*pow((dist(x,y,tcr.cx,tcr.cy)/radius-distmeanbs),2).toFloat
		}
		distvar /= pixelsum
		distvarbs /= pixelsumbs

		Map("minIntensity"->minint,"pixelSum"->pixelsum,"entropy"->entropy,"radcom"->radcom,"radskew"->radskew,"radvar"->radvar,
			"entropyPerPixel"->entropy/area, "distMean"->distmean, "distVariance"->distvar,
			"pixelSumBS"->pixelsumbs,"entropyBS"->entropybs,"radcomBS"->radcombs,"radskewBS"->radskewbs,"radvarBS"->radvarbs,
			"entropyPerPixelBS"->entropybs/area, "distMeanBS"->distmeanbs, "distVarianceBS"->distvarbs,
			"radgini"->radgini, "radginiBS"->radginibs
		)
	}
/*	def getMetricsOld(ip: ImageProcessor, bf: Circle, tcr: Circle): Map[String,Float]  = {
		val height = ip.getHeight
		val width = ip.getWidth
		val radius = tcr.r*2
		def dist(ax:Double,ay:Double,bx:Double,by:Double): Double = sqrt(pow(ax-bx,2)+pow(ay-by,2))

		if(!checkRadius(bf,tcr,width,height))
			return Map("pixelSum"->Double.NaN,"entropy"->Double.NaN,
				"entropyPerPixel"->Double.NaN, "distMean"->Double.NaN, "distVariance"->Double.NaN)
		var pixelsum = 0
		for(x <- 0 until width; y <- 0 until height){ pixelsum += ip.get(x,y) }
		val entropy = (for(x <-round(tcr.cx-tcr.r).toInt until round(tcr.cx+tcr.r).toInt; y <-round(tcr.cy-tcr.r).toInt until round(tcr.cy+tcr.r).toInt) yield {
			val p = ip.get(x,y) / pixelsum
			(if(p>0) -p * log(p) else 0)
		}).toArray.sum
		val distmean: Double = (for(x <-round(tcr.cx-tcr.r).toInt until round(tcr.cx+tcr.r).toInt; y <-round(tcr.cy-tcr.r).toInt until round(tcr.cy+tcr.r).toInt) yield {
			val p = ip.get(x,y)
			p*dist(x,y,tcr.cx,tcr.cy)
		}).toArray.sum / pixelsum / radius
		val distvar = (for(x <-round(tcr.cx-tcr.r).toInt until round(tcr.cx+tcr.r).toInt; y <-round(tcr.cy-tcr.r).toInt until round(tcr.cy+tcr.r).toInt) yield {
			val p = ip.get(x,y)
			p*pow((dist(x,y,tcr.cx,tcr.cy)-distmean),2)
		}).toArray.sum / pixelsum / radius

		Map("pixelSum"->pixelsum,"entropy"->entropy,
			"entropyPerPixel"->entropy/(width*height), "distMean"->distmean, "distVariance"->distvar)
	}
	*/
	def centerofmass(data: Array[Double]): Double = {
		val prob = normalize(data)
		val len = data.length
		val dist = (for(i <- 1 to len) yield ((1.0/len*i)-0.5/len)).toArray
		inner_product(prob,dist)
	}
	def variance(data: Option[Array[Double]]): Double = {
		data match {
			case Some(arr) => variance(arr)
			case None => Double.NaN
		}
	}
	def variance(data: Array[Double]): Double = {
		val len = data.length
		val dist = (for(i <- 1 to len) yield ((1.0/len*i)-0.5/len)).toArray
		val prob = normalize(data)
		val m = inner_product(prob,dist)
		val dev = dist.map(_-m)
		val dev2 = dev.map(pow(_,2))
		val ret = inner_product(prob,dev2)
		ret
	}
	def skewness(data: Option[Array[Double]]): Double = {
		data match {
			case Some(arr) => skewness(arr)
			case None => Double.NaN
		}
	}
	def gini(data: Option[Array[Double]]): Double = {
		data match {
			case Some(arr) => gini(arr)
			case None => Double.NaN
		}
	}
	def gini(data: Array[Double]): Double = {
		try{
			val	len = data.length
			val norm = normalize(data).sorted
			val linear =  (for(i <- 1 to len) yield (1.0/len*i)).toArray
			val accum = new Array[Double](len)
			for(i <- 0 until len){
				accum(i) =
					norm.slice(0,i+1).sum
			}
			val ret = (linear.sum - accum.sum)/linear.sum
			ret
		}catch{
			case e:Exception =>
				e.printStackTrace()
				return Double.NaN
		}
	}
	def normalize(arr: Array[Double]) = {
		val s = arr.sum
		arr.map(_/s)
	}
	def inner_product(a: Array[Double],b: Array[Double]): Double = a.zip(b).map(x=>x._1*x._2).sum
	def skewness(data: Array[Double]): Double = {
		val len = data.length
		val dist = (for(i <- 1 to len) yield ((1.0/len*i)-0.5/len)).toArray
		val prob = normalize(data)
		val m = inner_product(prob,dist)
		val dev = dist.map(_-m)
		val dev2 = dev.map(pow(_,2))
		val dev3 = dev.map(pow(_,3))
		val ret = inner_product(prob,dev3) / pow(inner_product(prob, dev2),1.5)
		ret
	}
	/*	def old_skewness(data: Array[Double]): Double = {
		//Incorrect!!
		def mean(arr: Array[Double]) = arr.sum/arr.length
		val i = 0
		val dev = data.map(_-mean(data))
		mean(dev.map(pow(_,3)))/pow(mean(dev.map(pow(_,2))),1.5)
	}*/
}

object ImgUtils{
	def getAdjustedImage(ip: ImageProcessor): BufferedImage = {
		val stat = ip.getStatistics
		val hist = stat.histogram
		val tail = round(0.01f * stat.pixelCount)
		var count = 0
		var minhist = -1
		var maxhist = -1
		for(i <- 0 until 256){
			count += hist(i)
			if(count>tail && minhist == -1)
				minhist = i
			else if(count > stat.pixelCount - tail && maxhist == -1)
				maxhist = i
		}
		val range = stat.histMax-stat.histMin
		val max = round(stat.histMin + maxhist/256*range)
		val min = round(stat.histMin + minhist/256*range)
		ip.setMinAndMax(min,max)
		ip.getBufferedImage
	}
}