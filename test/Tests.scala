package test

import hk.CellImageAnalyzer._
import hk._
import ij.process.{ImageProcessor, ShortProcessor}
import ij.{IJ, ImagePlus}
import org.scalatest._
import matchers.ShouldMatchers
import math._
import prop.PropertyChecks
import util.Random

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.Some
import scala.Some
import hk.Circle

/**
 * Created with IntelliJ IDEA.
 * User: hiroyuki
 * Date: 12/5/12
 * Time: 11:42 AM
 * To change this template use File | Settings | File Templates.
 */


//For now, tests will be done only for radvarMirror, which will be used for the paper.

@RunWith(classOf[JUnitRunner])
class RadialProfileMetricsTest extends FunSuite with ShouldMatchers {
	import RadialProfileMetrics._

	test("Inner product") {
		//Norm (squared)
		val rand = new Random
		val a: Array[Double] = Array.tabulate(20){i:Int => {rand.nextDouble % 1000d}}
		val prod = inner_product(a,a)
		val sqsum: Double = a.map(v => v * v).sum

		assert(sqsum === prod)
	}

	test("Mirror image of arbitrary radial profile (RP) gives zero average and skewness.") {
		val rand = new Random
		for(i<-0 until 100) {
			//Sound assumption: # of elements is 20, intensity is less than 1000
			val radial: Array[Double] = Array.tabulate(20){i:Int => {rand.nextDouble % 1000d}}
			val radMirror = mirror(radial)
//			println(radMirror.map("%.2f".format(_)).mkString(" "))
			val w: Double = rand.nextDouble % 2d + 0.5d

			centerofmass(radMirror,-w,w) should be < 0.0001d
			skewness(radMirror,-w,w) should be < 0.0001d
		}
	}
	//ToDo: Add a test for variance using a gaussian.
	// Also gini coefficient?
}

@RunWith(classOf[JUnitRunner])
class ImageAnalyzerSpec extends FunSuite with BeforeAndAfter with ShouldMatchers {
	import CellImageAnalyzer._
	import ImageAnalyzerSpec._
	val rand = new MyRandom

	test("dist") {
		val rand = new Random
		for(i<- 0 until 100){
			val x = rand.nextFloat % 1000f
			val y = rand.nextFloat % 1000f
			val d = rand.nextFloat % 1000f - 500f

			dist(x,y,x,y) should be < 0.01f
			(dist(x,y,x+d,y) - abs(d)) should be < 0.01f
			(dist(x,y,x,y+d) - abs(d)) should be < 0.01f
			(dist(x+d,y,x,y) - abs(d)) should be < 0.01f
			(dist(x,y+d,x,y) - abs(d)) should be < 0.01f
		}
	}

	test("test of rotation test functions") {
		for(i <- 0 until 100){
			val w = rand.doubleInRange(100,300).toFloat
			val x = rand.doubleInRange(50,w).toFloat
			val y = rand.doubleInRange(100,300).toFloat
			//360 deg rotation, 90deg x 4 times
			val rot360: (Float,Float) = rotateTuple(rotateTuple(
										rotateTuple(rotateTuple((x,y),w),w),w),w)
			distTuple((x,y),rot360) should be < 0.001f
		}
	}
/*
	test("centroid rotation") {
		for (i <- 0 until 100) {
			val r: Int = abs(rand.nextInt) % 100 + 50
			val w: Int = r * 2 + 1
			val imp = mkGaussianWithNoise(w,w)
			val circle = new Circle(r,r,r)

			val p = (imp,circle)
			val p2 = rotateLeft(p)
			val p3 = rotateLeft(p2)
			val p4 = rotateLeft(p3)
/*			p._1.show()
			p2._1.show()
			p3._1.show()
			p4._1.show() */

			val c = centroid(p._1.getProcessor,p._2)
			val c2 = centroid(p2._1.getProcessor,p2._2)
			val c3 = centroid(p3._1.getProcessor,p3._2)
			val c4 = centroid(p4._1.getProcessor,p4._2)
			println(c,c2,c3,c4)

			distTuple(rotateTuple(c,w),c2) should be < 0.01f
			distTuple(rotateTuple(c2,w),c3) should be < 0.01f
			distTuple(rotateTuple(c3,w),c4) should be < 0.01f
			distTuple(rotateTuple(c4,w),c) should be < 0.01f
		}
	}*/
}

//@RunWith(classOf[JUnitRunner])
class ImageAnalyzerSpecOld extends FunSuite with BeforeAndAfter {
	import ImageAnalyzerSpec._
	val maxRadius = 300
	val minRadius = 50

	var imgs: Array[ImagePlus] = _
	var params: Array[(ImagePlus,Circle)] = _

	//Make a param set before every test
	before {
		// ToDo: change imgs to real cell images.
		imgs = loadImages //++ Array(ImageAnalyzerSpec.mkRandomImg(1200,800),ImageAnalyzerSpec.mkRandomImg(1000,700))
		val rand = new Random {
			def intInRange(min: Int, max: Int): Int = {
				this.nextInt(max-min+1) + min
			}
		}
		//Direct product of a set of images and a set of different regions
		params = (for (im <- imgs) yield {
			for(i <- 0 until 100) yield {
				val w = im.getWidth
				val h = im.getHeight
				val r = rand.intInRange(minRadius,maxRadius)
				val cx = rand.intInRange(r,w-r-1)
				val cy = rand.intInRange(r,h-r-1)
				(im,new Circle(cx,cy,r))
			}
		}).flatten
	}

	test("metrics with subtracted BG is constant when BG is added/subtracted.") {
		for(p <- params) {
			val ip = p._1.getProcessor
			val c = p._2
			ip.setRoi(round(c.cx-c.r),round(c.cy-c.r),round(c.r*2+1),round(c.r*2+1))
			val ipcrop = ip.crop
			val addNum: Int = 0 - ipcrop.minValue.toInt + 1  //Any value in [0..minValue] should be fine.

			val p2 = addValue(p,addNum) //ToDo: make sure this does not overflow.

			val (rad1,met1) = getMetrics(p)
			val (rad2,met2) = getMetrics(p2)
			val rmet1 = intensitiShiftInvariant(met1)
			val rmet2 = intensitiShiftInvariant(met2)
			assert(rmet1 === rmet2)
		}
	}

	test("normalized radial profile should be an invariant over scaling"){
		for(p <- params) {
			val p2 = scale(p,1.5f)
			val (rad1,met1) = getMetrics(p)
			val (rad2,met2) = getMetrics(p2)
			assert(twoRadialSame(rad1,rad2))
		}
	}

	test("All metrics except 'momentxy' and radial profile should be invariants over rotation."){
		//Here only 90, 180, 270 degree rotation is tested.
		for(p <- params) {
			val p2 = rotateLeft(p)
			val p3 = rotateLeft(p2)
			val p4 = rotateLeft(p3)

			val (rad1,met1) = getMetrics(p)
			val (rad2,met2) = getMetrics(p2)
			val (rad3,met3) = getMetrics(p3)
			val (rad4,met4) = getMetrics(p4)

			val rmet1 = rotationInvariant(met1)
			val rmet2 = rotationInvariant(met2)
			val rmet3 = rotationInvariant(met3)
			val rmet4 = rotationInvariant(met4)

			assert(twoRadialSame(rad1,rad2))
			assert(twoRadialSame(rad1,rad3))
			assert(twoRadialSame(rad1,rad4))

			assert(rmet1 === rmet2)
			assert(rmet1 === rmet3)
			assert(rmet1 === rmet4)
		}
	}



	test("statistics from images should not depend on the order of collecting radial") {
		pending
	}

	test("random image should give small abs value of mean position") {
		pending
	}
}

case class Prop(val name: String, val rotationInvariant: Boolean, val intensityShiftInvariant: Boolean)

object Prop {
	def fromTuple(t: (String,Boolean,Boolean)): Prop = {
		new Prop(t._1,t._2,t._3)
	}
}

class MyRandom extends Random {
	//Random Int in range (inclusive)
		def intInRange(min: Int, max: Int): Int = {
			require(min < max)
			this.nextInt(max-min+1) + min
		}

	def doubleInRange(min: Double, max: Double): Double = {
		require(min < max)
		this.nextDouble % (max-min) + min
	}
}
object ImageAnalyzerSpec {
	val rand = new MyRandom
	def twoRadialSame(a:Option[Array[Double]],b:Option[Array[Double]]): Boolean = {
		(a,b) match {
			case (Some(as),Some(bs)) => {
				println(as.map("%.2f".format(_)).mkString(" "))
				println(bs.map("%.2f".format(_)).mkString(" "))
				as.length == bs.length && as.zip(bs).forall(v => v._1==v._2 )
			}
			case _ => false
		}
	}
	def loadImages: Array[ImagePlus] = {
		val baseFolder = "/Users/hiroyuki/Dropbox/Groves Lab Data/Scope Pics/TCR nanodot all fixed image radial set used in the paper/20111213_npat/FC01_40nm_1--10_ag--null/nopat/"
		val paths: Array[String] = (1 to 20).map(baseFolder+"%d.stk".format(_)).toArray
		paths.map(IJ.openImage)
	}

	val metricsProps: Map[String,Prop] = Array(
		("minIntensity",true,false),
		("pixelSum",true,false),
		("pixelSumBS",true,true),
		("entropy",true,false),
		("entropyBS",true,true),
		("entropyPerPixel",true,false),
		("entropyPerPixelBS",true,true),
		("distMean",true,false),
		("distMeanBS",true,true),
		("distVariance",true,false),
		("distVarianceBS",true,true),
		("variance",true,false),
		("varianceBS",true,true),
		("moment2nd",true,false),
		("momentx2py2",true,false),
		("momentxy",false,false),
		("moment2ndBS",true,true),
		("momentx2py2BS",true,true),
		("momentxyBS",true,true),
		("radcom",true,false),
		("radskew",true,false),
		("radvar",true,false),
		("radcomBS",true,true),
		("radskewBS",true,true),
		("radvarBS",true,true),
		("radgini",true,false),
		("radginiBS",true,true),
		("radvarMirror",true,false),
		("radvarMirrorBS",true,true)
	).map(a => {(a._1,Prop.fromTuple(a))}).toMap

	def rotationInvariant(met: Map[String,Float]): Map[String,Float] = {
		met.filter(m => {metricsProps(m._1).rotationInvariant})
	}

	def intensitiShiftInvariant(met: Map[String,Float]): Map[String,Float] = {
		met.filter(m => {metricsProps(m._1).intensityShiftInvariant})
	}

	def mkGaussianWithNoise(w:Int,h:Int): ImagePlus = {
		require(w > 0 && h > 0)
		val img = IJ.createImage("test","16-bit",w,h,1)
		val ip: ShortProcessor = img.getProcessor.asInstanceOf[ShortProcessor]
		val arr = ip.getPixels.asInstanceOf[Array[Short]]
		val center: (Int,Int) = (rand.intInRange(0,w),rand.intInRange(0,h))
		val sigma: Double = rand.doubleInRange(min(w,h)/2,min(w,h))
		val height: Double = rand.doubleInRange(100d,1000d)
		for(x <- 0 until w) {
			for(y <- 0 until h) {
				val v: Double = height * exp((0-pow(x-center._1,2)-pow(y-center._2,2))/(2d*sigma*sigma))
				val noise: Double = rand.doubleInRange(0d,height/20)
				ip.set(x,y,round(v+noise).toInt)
			}
		}
		img
	}
	//Make a random 16bit image (ShortProcessor) with a specified dimension.
	def mkRandomImg(w:Int,h:Int): ImagePlus = {
		require(w > 0 && h > 0)
		val img = IJ.createImage("test","16-bit",w,h,1)
		val ip: ShortProcessor = img.getProcessor.asInstanceOf[ShortProcessor]
		val arr = ip.getPixels.asInstanceOf[Array[Short]]
		for (i <- 0 until w * h) {
			arr(i) = (rand.nextInt % Short.MaxValue).toShort
		}
		img
	}
	def rotateLeft(p: (ImagePlus, Circle)): (ImagePlus, Circle) = {
		val ip = p._1.getProcessor
		val w = ip.getWidth
//		val h = ip.getHeight
		val c = p._2

		val ipnew = ip.rotateLeft
		val cnew = new Circle(c.cy,w - c.cx,c.r)

		(new ImagePlus("rotated",ipnew),cnew)
	}

	def rotateTuple(p: (Float,Float),width: Float): (Float,Float) = (p._2,width - p._1 - 1)

	def scale(p: (ImagePlus, Circle), factor: Float): (ImagePlus, Circle) = {
		val ip = p._1.getProcessor
		val c = p._2

		val ipnew: ImageProcessor = ip.duplicate
		ipnew.scale(factor,factor)
		val cnew = new Circle(factor*c.cx,factor*c.cx,factor*c.r)
		(new ImagePlus("scaled",ipnew),cnew)
	}
	def addValue(p: (ImagePlus, Circle), value: Int): (ImagePlus, Circle) = {
		val ip = p._1.getProcessor
		val c = p._2

		val ipnew: ImageProcessor = ip.duplicate
		ipnew.add(value)
		val cnew = new Circle(c.cx,c.cx,c.r)
		(new ImagePlus("added",ipnew),cnew)
	}

	def getMetrics(p: (ImagePlus, Circle)): (Option[Array[Double]],Map[String,Float]) = {
		val ip = p._1.getProcessor
		val boundary = p._2
		val center = CellImageAnalyzer.calcTcrCenter(ip,boundary,TcrAlgorithm.CoM3subtractBG)
		val tcr = new Circle(center.x,center.y,boundary.r)
		val radial = getRadialProfile(ip,boundary,tcr)
		val metrics = getTcrMetrics(ip,boundary,tcr,radial)
		(radial, metrics)
	}

	def distTuple(p1: (Float,Float), p2: (Float,Float)): Float
		= sqrt(pow(p1._1 - p2._1,2) + pow(p1._2 - p2._2,2)).toFloat
}