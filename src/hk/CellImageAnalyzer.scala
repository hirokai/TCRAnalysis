package hk

import scala.math._
import java.awt.image.BufferedImage
import ij.process.ImageProcessor
import TcrAlgorithm._
import ij.ImagePlus
import ij.gui.{OvalRoi, Roi}

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
		xmin >= 0 && xmax < width && ymin >= 0 && ymax < height
	}

	def mkCircleRoi(c: Circle): OvalRoi = new OvalRoi(round(c.cx-c.r),round(c.cy-c.r),round(c.r*2),round(c.r*2))

	//12/11/2012: Changed to use a built-in statistics function.
	def centroid(ip: ImageProcessor, area: Circle, minint: Float = 0f): (Float,Float) = {
		val roi:Roi = mkCircleRoi(area)
		ip.setRoi(roi)
		val stat = ip.getStatistics
		(stat.xCentroid.toFloat,stat.yCentroid.toFloat)
	}

	//This assumes that image is not normalized.
	//No normalization will be done.
	def centralMoment(ip: ImageProcessor, area: Circle, cx: Float, cy: Float, minint: Float = 0f)(r: Int, s: Int): Float = {
		var sum = 0f
		for (x <-round(area.cx-area.r) until round(area.cx+area.r);
		     y <-round(area.cy-area.r) until round(area.cy+area.r) if dist(x,y,area.cx,area.cy)<area.r) {
			val p = ip.get(x,y) - minint
			sum += pow(x-cx,r).toFloat * pow(y-cy,s).toFloat * p.toFloat
		}
		sum
	}

	val availableMetrics: Map[String,String] =
		Map("entropy"->"Entropy","radcom"->"CoM","radskew"->"Skewness",
			"entropyPerPixel"->"Entropy per pixel", "distMean"->"Mean distance", "distVariance"->"Distance variance",
			"pixelSumBS"->"Total intensity (BG subtracted)","entropyBS"->"Entropy (BG subtracted)","radcomBS"->"CoM (BG subtracted)",
			"radskewBS"->"Skewness (BG subtracted)","entropyPerPixelBS"->"Entropy per pixel (BG subtracted)",
			"distMeanBS"->"Mean distance (BG subtracted)", "distVarianceBS"->"Distance variance (BG subtracted)",
			"radgini"->"Gini coefficient", "radginiBS"->"Gini coefficient (BG subtracted)",
		//Following was added on Nov 2012
		  "radvarMirror" -> "Variance of centrosymmetric radial profile",
			"radvarMirrorBS" -> "Variance of centrosymmetric radial profile (BG subtracted)",
			"variance" -> "Variance of 2D image",  //See a keynote slide or something for more detailed explanation.
			"varianceBS" -> "Variance of 2D image (BG subtracted)",
			"moment2nd" -> "Invariant 2nd order central moment",
			"momentx2py2" -> "mu_20 + mu_02",
		  "momentxy" -> "Moment mu_11",
			"moment2ndBS" -> "Invariant 2nd order central moment (BG subtracted)",
			"momentx2py2BS" -> "mu_20 + mu_02 (BG subtracted)",
			"momentxyBS" -> "Moment mu_11 (BG subtracted)"
	)

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

	def getMinimumIntensityInArea(ip: ImageProcessor, c: Circle): Int = {
		val imp = new ImagePlus("temp",ip)
		val roi = new OvalRoi(round(c.cx - c.r), round(c.cy - c.r),round(c.r*2),round(c.r*2))
		imp.setRoi(roi)
		imp.getStatistics.min.toInt
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
				getMinimumIntensityInArea(ip,new Circle(bf_center.x,bf_center.y,radius.toFloat))
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
		val area:Float = (radius*radius*Pi).toFloat

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
		val (radcom,radcombs,radskew,radskewbs,radvar,radvarbs,radgini,radginibs,radvarMirror,radvarMirrorbs):
					(Float,Float,Float,Float,Float,Float,Float,Float,Float,Float) =
			radial match {
			case Some(r) => {
				import RadialProfileMetrics._
				val r_bs = r.map(v => {(v - minint)})
				(centerofmass(r).toFloat,centerofmass(r_bs).toFloat,
				 skewness(r).toFloat,centerofmass(r_bs).toFloat,
				 variance(r).toFloat,variance(r_bs).toFloat,
				 gini(r).toFloat, gini(r_bs).toFloat,
				 variance(mirror(r),-1,1).toFloat,variance(mirror(r_bs),-1,1).toFloat
				)
			}
			case None => {
				(Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN, Float.NaN)
			}
		}

		//
		//Metrics that are directly calculated from image
		//

		var entropy = 0f
		var entropybs = 0f
		var distmean = 0f
		var distmeanbs = 0f
		var mean_r = 0f

		// Calculate entropy and mean distance (distmean) first.
		// entropybs and distmeanbs are based on background-subtracted image.
		// Iterate for the area inside the circle defined by tcr.
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p: Int = ip.get(x,y)
			val pb = p - minint
			val pn: Float = p / pixelsum   //Pixel intensity normalized so that the sum inside the circle is 1
			val pbn = pb / pixelsumbs
			val r: Float = dist(x,y,tcr.cx,tcr.cy).toFloat
			val r_signed: Float = r * (signum(x-tcr.cx) match {
				case 1 => 1
				case -1 => -1
				case 0 => (-1) * signum(y-tcr.cy)
			})

			//Use p instead of pn if possible, in order to avoid error due to addition of small float many times.
			//Int should not overflow unless image region is huge.
			entropy += (if(pn>0) -pn * log(pn) else 0).toFloat
			entropybs += (if(pbn>0) -pbn * log(pbn) else 0).toFloat
			distmean += p*r
			distmeanbs += pb*r
			mean_r += p * r_signed
		}
		//Here all of these three are normalized to [0,1]
		distmean /= (radius * pixelsum)
		distmeanbs /= (radius * pixelsumbs)
		mean_r /= (radius * pixelsum)

		//Calculate variance of distance (distvar) using distmean that was calculated above.
		var distvar = 0f
		var distvarbs = 0f
		var rvariance = 0f
		var rvariancebs = 0f
		for(x <-round(tcr.cx-tcr.r) until round(tcr.cx+tcr.r); y <-round(tcr.cy-tcr.r) until round(tcr.cy+tcr.r) if dist(x,y,tcr.cx,tcr.cy)<radius){
			val p = ip.get(x,y)
			val pb = p - minint
			val r: Float = dist(x,y,tcr.cx,tcr.cy).toFloat
			distvar += p*pow((r/radius-distmean),2).toFloat
			distvarbs += pb*pow((r/radius-distmeanbs),2).toFloat
			val r_signed: Float = r * (signum(x-tcr.cx) match {
				case 1 => 1
				case -1 => -1
				case 0 => (-1) * signum(y-tcr.cy)
			})
			rvariance += pow(r_signed-mean_r,2).toFloat*p
			rvariancebs += pow(r_signed-mean_r,2).toFloat*pb
		}
		distvar /= pixelsum
		distvarbs /= pixelsumbs
		rvariance /= radius * pixelsum
		rvariancebs /= radius * pixelsumbs


		//Added on Nov, 2012.
		//Caculate some central moments
		// http://www.isl.titech.ac.jp/~nagahashilab/member/longb/iip/LectureNotes/lecture3.pdf
		// http://www.tandfonline.com/doi/pdf/10.1080/02664768900000051
		// http://goo.gl/bQQkE
		// http://www.sci.utah.edu/~gerig/CS7960-S2010/handouts/CS7960-AdvImProc-MomentInvariants.pdf


		//These are separate parts, since calculation is possibly done
		// in a circle around a manually picked point (bf, from BF image).

		// First, calculate a centroid.
		val (cx,cy):(Float,Float) = centroid(ip,tcr)
		// Then, calculate central moments
		val central = centralMoment(ip,tcr,cx,cy)_
		val mu20 = central(2,0)
		val mu02 = central(0,2)
		val mu00 = central(0,0)

		//This eta1 is a shape invariant (Remains constant under translation, rotation, similarity transformation)
		val eta1: Float = (mu20 + mu02) / pow(mu00,2).toFloat
		val mu20_p_mu02 = mu20 + mu02
		val mu11 = central(1,1)

		//The same thing but with BG subtraction.
		val (eta1BS,mu20_p_mu02BS,mu11BS) = {
			//Use the same minint as before: THIS SHOULD BE CHANGED WHEN USE OTHER AREA THAN tcr
			val (cx,cy):(Float,Float) = centroid(ip,tcr,minint)
			// Then, calculate central moments
			val centralbg = centralMoment(ip,tcr,cx,cy,minint)_
			val mu20 = centralbg(2,0)
			val mu02 = centralbg(0,2)
			val mu00 = centralbg(0,0)

			//This eta1 is a shape invariant (Remains constant under translation, rotation, similarity transformation)
			val eta1: Float = (mu20 + mu02) / pow(mu00,2).toFloat
			val mu20_p_mu02 = mu20 + mu02
			val mu11 = centralbg(1,1)
			(eta1,mu20_p_mu02,mu11)
		}


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
			"variance" -> rvariance,"varianceBS" -> rvariancebs,
			"moment2nd" -> eta1,"momentx2py2" -> mu20_p_mu02 , "momentxy" -> mu11,
			"moment2ndBS" -> eta1BS, "momentx2py2BS" -> mu20_p_mu02BS , "momentxyBS" -> mu11BS,
				//Metrics from radial profile
			"radcom"->radcom,"radskew"->radskew,"radvar"->radvar,
			"radcomBS"->radcombs,"radskewBS"->radskewbs,"radvarBS"->radvarbs,
			"radgini"->radgini, "radginiBS"->radginibs,
			"radvarMirror" -> radvarMirror, "radvarMirrorBS" -> radvarMirrorbs
		)
	}
}

/**
 * Calculate metrics from radial profile.
 * Radial profile is calculated by CellImageAnalyzer
 */
//This class has a test suite at test/Tests.scala
object RadialProfileMetrics{
	def inner_product(a: Array[Double],b: Array[Double]): Double = a.zip(b).map(x=>x._1*x._2).sum
	def normalize(arr: Array[Double]) = {
		val s = arr.sum
		arr.map(_/s)
	}
	//Radial proifile metrics
	def centerofmass(data: Array[Double],min:Double=0,max:Double=1): Double = {
		val prob = normalize(data)
		val len = data.length
		val dist = mkXCoord(len,min,max)
		inner_product(prob,dist)
	}

	def mkXCoord(len: Int, min: Double, max: Double): Array[Double] = {
		require(min < max)
		require(len > 0)
		val interval = (max-min)/len
		val dist = (0 until len).map(i => {i*interval + interval*0.5 + min}).toArray
		dist
	}

	def variance(data: Array[Double],min:Double=0,max:Double=1): Double = {
		val len = data.length
		val dist = mkXCoord(len,min,max)
		val prob = normalize(data)
		val m = inner_product(prob,dist)
		val dev = dist.map(_-m)
		val dev2 = dev.map(pow(_,2))
		val ret = inner_product(prob,dev2)
		ret
	}

	def mirror(radial: Array[Double]): Array[Double] = radial.reverse ++ radial

	def skewness(radial: Array[Double], min: Double = 0, max: Double = 1): Double = {
		val dist = mkXCoord(radial.length,min,max)
		val prob = normalize(radial)
		val m = inner_product(prob,dist)
		val dev = dist.map(_-m)
		val dev2 = dev.map(pow(_,2))
		val dev3 = dev.map(pow(_,3))
		val ret = inner_product(prob,dev3) / pow(inner_product(prob, dev2),1.5)
		ret
	}

	//Does not have a test code yet.
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
		var minhist = -1f
		var maxhist = -1f
		for(i <- 0 until 256){
			count += hist(i)
			if(count>tail && minhist == -1)
				minhist = i
			else if(count > stat.pixelCount - tail && maxhist == -1)
				maxhist = i
		}
		val range = stat.histMax-stat.histMin
		val max = round(stat.histMin + maxhist/256f*range)
		val min = round(stat.histMin + minhist/256f*range)
		ip.setMinAndMax(min,max)
		ip.getBufferedImage
	}
}