package hk

import scala.math._
import java.awt.image.BufferedImage
import ij.process.ImageProcessor
import TcrAlgorithm._

/**
 * ToDo: ImageProcessor holds value as Float, so conversion between double and float is cumbersome.
 */

object CellImageAnalyzer{

	def dist(ax:Float,ay:Float,bx:Float,by:Float): Float = sqrt(pow(ax-bx,2)+pow(ay-by,2)).toFloat
	def checkRadius(bf:Circle, tcr:Circle,width:Int,height:Int): Boolean = {
		val xmin = min(bf.cx-bf.r,tcr.cx-tcr.r)
		val xmax = max(bf.cx+bf.r,tcr.cx+tcr.r)
		val ymin = min(bf.cy-bf.r,tcr.cy-tcr.r)
		val ymax = max(bf.cy+bf.r,tcr.cy+tcr.r)
		xmin>=0 && xmax < width && ymin >= 0 && ymax < height
	}

	val availableMetrics: Map[String,String] =
		Map("entropy"->"Entropy","radcom"->"CoM","radskew"->"Skewness",
			"entropyPerPixel"->"Entropy per pixel", "distMean"->"Mean distance", "distVariance"->"Distance variance",
			"pixelSumBS"->"Total intensity (BG subtracted)","entropyBS"->"Entropy (BG subtracted)","radcomBS"->"CoM (BG subtracted)",
			"radskewBS"->"Skewness (BG subtracted)","entropyPerPixelBS"->"Entropy per pixel (BG subtracted)",
			"distMeanBS"->"Mean distance (BG subtracted)", "distVarianceBS"->"Distance variance (BG subtracted)",
			"radgini"->"Gini coefficient", "radginiBS"->"Gini coefficient (BG subtracted)")

	val emptyMetricsResult: Map[String,Float] = availableMetrics.keys.map(k => {k -> Float.NaN}).toMap
	def getRadialProfile(ip: ImageProcessor, bf: Circle, tcr: Circle, num_bins: Int = -1): Option[Array[Double]]  = {
		val height = ip.getHeight
		val width = ip.getWidth
		def dist(ax:Double,ay:Double,bx:Double,by:Double) = sqrt(pow(ax-bx,2)+pow(ay-by,2))

		val numBin = if(num_bins>0) num_bins else Config.numBin
		//var index = 0
		//val intensity = new Array[Double](numBin)
		if(!checkRadius(bf,tcr,width,height)){
			None
		}else{
			val count = new Array[Int](numBin)
			val sum = new Array[Int](numBin)
			val rint = tcr.r / numBin
			for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r)){
				val bin = (floor(dist(tcr.cx,tcr.cy,x,y)/rint)).toInt
				if(bin<numBin){
					count(bin) +=  1
					sum(bin) += ip.get(x,y)
				}
			}
			val ret = sum.zip(count).map(a=>a._1.toDouble/a._2)
			Some(ret)
		}
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

	// If radial is None, some metrics such as radskew will be NaN,
	// but some other metrics that are calculated directly from image intensity ("image-based metrics") may still be valid.
	// checkRadius() determines if metrics can be calculated or not.
	// Currently, criteria for whether radial profile is valid and whether image-based metrics are valid are the same.
	def getTcrMetrics(ip: ImageProcessor, bf: Circle, tcr: Circle, radial: Option[Array[Double]]): Map[String,Float]  = {

		//Radius and area
		val radius:Float = tcr.r
		val area:Float = (radius*radius*3.14159).toFloat

		if(!checkRadius(bf,tcr,ip.getWidth,ip.getHeight))
			return emptyMetricsResult

		//Calculate minimum intensity and sum of intensity.
		var minint: Int = ip.get(round(tcr.cx).toInt,round(tcr.cy).toInt)
		var pixelsum = 0f
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p = ip.get(x.toInt,y.toInt).toInt
			pixelsum += p
			minint = min(minint,p)
		}
		val pixelsumbs = pixelsum - minint*area

		//Metrics based on radial profile
		//Functions are defined in RadialProfileMetrics
		val (radcom,radcombs,radskew,radskewbs,radvar,radvarbs,radgini,radginibs) = radial match {
			case Some(r) => {
				import RadialProfileMetrics._
				(centerofmass(r).toFloat,centerofmass(r.map(v => {(v - minint)})).toFloat,
				 skewness(r).toFloat,centerofmass(r.map(v => {(v - minint)})).toFloat,
				 variance(r).toFloat,variance(r.map(_-minint)).toFloat,
				 gini(r).toFloat, gini(r.map(_-minint)).toFloat)
			}
			case None => {
				(Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN)
			}
		}

		//
		//Metrics that are directly calculated from image
		//

		var entropy = 0f
		var entropybs = 0f
		var distmean = 0f
		var distmeanbs = 0f

		// Calculate entropy and mean distance (distmean) first.
		// entropybs and distmeanbs are based on background-subtracted image.
		// Iterate for the area inside the circle defined by tcr.
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

		//Calculate variance of distance (distvar) using distmean that was calculated above.
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

		//Return all metrics calculated in this function.
		Map(
			//Metrics that are calculated directly from image
			//Intensity
			"minIntensity"->minint,"pixelSum"->pixelsum,"pixelSumBS"->pixelsumbs,
			//Entropy
			"entropy"->entropy, "entropyBS"->entropybs, "entropyPerPixel"->entropy/area,"entropyPerPixelBS"->entropybs/area,
			//Mean distance from center
			"distMean"->distmean, "distMeanBS"->distmeanbs,
			//Variance of distance from center
			"distVariance"->distvar, "distVarianceBS"->distvarbs,

			//Metrics from radial profile
			"radcom"->radcom,"radskew"->radskew,"radvar"->radvar,
			"radcomBS"->radcombs,"radskewBS"->radskewbs,"radvarBS"->radvarbs,
			"radgini"->radgini, "radginiBS"->radginibs
		)
	}


}

/**
 * Calculate metrics from radial profile.
 * Radial profile is calculated by CellImageAnalyzer
 */
object RadialProfileMetrics{
	def inner_product(a: Array[Double],b: Array[Double]): Double = a.zip(b).map(x=>x._1*x._2).sum
	def normalize(arr: Array[Double]) = {
		val s = arr.sum
		arr.map(_/s)
	}
	//Radial proifile metrics
	def centerofmass(data: Array[Double]): Double = {
		val prob = normalize(data)
		val len = data.length
		val dist = (for(i <- 1 to len) yield ((1.0/len*i)-0.5/len)).toArray
		inner_product(prob,dist)
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