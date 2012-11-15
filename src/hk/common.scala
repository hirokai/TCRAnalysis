package hk

import scala.math._
import java.awt.geom.Point2D
import java.awt.geom.Point2D.{Float => PFloat}

object TcrAlgorithm extends Enumeration {
  type TcrAlgorithm = Value 
  val CoM, CoM3, CoMsubtractBG, CoM3subtractBG = Value
  val name = Map(CoM->"Center of Mass",CoM3->"CoM 3rd power",
      CoMsubtractBG->"CoM BG subtracted",CoM3subtractBG->"CoM3 BG subtracted")
}

object CellPickMethod extends Enumeration {
  type CellPickMethod = Value 
  val CircleOrigin,CircleRect = Value
  val name = Map(CircleOrigin->"Origin and radius",CircleRect->"Rect")
}

object Config {
  var calcTcrCenter: Boolean = true
  var tcrCenterAlgorithm = TcrAlgorithm.CoM3subtractBG
  val filename = new Con4[String](bf="img_000000000_dia_000.tif",
      ricm="img_000000000_RICM_000.tif",
      tcr="img_000000000_TR epi_000.tif",
      icam="img_000000000_GFP epi_000.tif")
  var autoSave = false
  //imgType 1:Scope7, 2: Scope2 
  var imgType = -1
  //bum of bins of radial profile
  var numBin = 20
  var shuffleMode = false
  var batchMode = false
  var cellPickMethod = CellPickMethod.CircleRect 
}

case class Circle(cx: Float, cy: Float, r: Float){
  override def toString = "(%.1f, %.1f, %.1f)" format (cx,cy,r)
  def isInside(p: Point2f) = p.distsq(new Point2f(cx,cy)) <= pow(r,2)
  def cxi: Int = round(cx)
  def cyi: Int = round(cy)
}

class Con4[A](var bf: A = null, var ricm: A = null, var tcr: A = null, var icam: A = null) extends Iterable[A]{
/*	override def foreach[U](f: A => U): Unit = {
	   f(bf); f(ricm);f(tcr);f(icam)
	}
*/

  def iterator: Iterator[A] = new Iterator[A]{
    var index = -1
    def next(): A = {
      index += 1
      index match {
        case 0 => bf
        case 1 => ricm
        case 2 => tcr
        case 3 => icam
        case _ => throw new NoSuchElementException
      }
    }
    def hasNext: Boolean = index<3
  }
   def this(a: IndexedSeq[A]) = {
    this(null.asInstanceOf[A],null.asInstanceOf[A],null.asInstanceOf[A],null.asInstanceOf[A])
    if(a.length!=4) throw new IllegalArgumentException
    bf=a(0); ricm=a(1); tcr = a(2); icam = a(3)
  }
}

/**
 * This holds Float values. Float is compatible with ImageProcessor (Double isn't, so stick to Float)
 * @param x x coordinate
 * @param y y coordinate
 */
class Point2f(val x: Float, val y:Float){
  def xi: Int = round(x).toInt
  def yi: Int = round(y).toInt
  def dist(p: Point2f) = sqrt(pow(x-p.x,2)+pow(y-p.y,2))
  def distsq(p: Point2f) = pow(x-p.x,2)+pow(y-p.y,2)
  override def toString:String = "(%.1f,%.1f)".format(x,y)
}

object Point2f{
  implicit def fromPoint2D(p: Point2D.Float): Point2f = new Point2f(p.x,p.y)
  implicit def toPoint2D(p: Point2f):Point2D.Float  = new PFloat(p.x,p.y)
}

object Utils {
	def printStatus(msg: String) {
		//Stub!! I want to show the message in lbStatus
		println(msg)
	}
}
