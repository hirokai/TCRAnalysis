package hk;

import scala.math._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io.Source
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt._
import scala.xml._
import ij.process.ImageProcessor
import scala.collection.mutable.HashMap

object CellDataSet {
  def newDataSet(m: Path,imageLoad: Boolean = true): CellDataSet = {
    def nameCheck(m:Path) = Array("metrics.txt","metrics_mod.txt","celldata.xml").contains(m.name)
	  if(!nameCheck(m)){
		  println(m)
		  throw new Exception("CellDataSet.newDataSet: metrics file name invalid.")
	  }
	  val ds = new CellDataSet
	  if(imageLoad)
		  ds._img = ImageManager.createImageManager(m,Config.imgType).getOrElse(null)
	  ds.readMetricsFromFolder(m.parent)
	  ds
  }
	def createEmptyDataset: CellDataSet = {
    new CellDataSet
  }
}

class CellDataSet {
  var metricsFile: Path = _
  private var _img: ImageManager = _
  def img: ImageManager = if(_img != null) {
	  _img
  } else {
	  _img = ImageManager.createImageManager(metricsFile,Config.imgType).getOrElse(null)
	  _img
  }

  private var _cells = new ArrayBuffer[Cell]
  def cells = _cells.toArray
  var observers = ArrayBuffer[CellDataObserver]()

  override def finalize() {
    if(img!=null) img.release()
  }
  def removeCell(cs: Array[Cell]) {
	  _cells --= cs
      if(cs.length>0)
    	observers.foreach(_.cellRemoved(cs))
  }
  def addCell(cx:Float,cy:Float,r:Float) {
    val c = new Cell
    c.boundary = new Circle(cx,cy,r)
    addCell(Array(c))
  }
  def addCell(cs:Array[Cell]) {
      cs.foreach({c=>
       calcCellMetrics(c,(c.tcrCenter==null)) 
      })
    _cells ++= cs
    if(cs.length>0)
    	observers.foreach(_.cellAdded(cs))
  }
  def calcCellMetrics(cell: Cell, calcTcr:Boolean = true){
    import CellImageAnalyzer._
    //Assuming boundary input.
    var tcr: Circle = null
    if(calcTcr || cell.tcrCenter==null){
    	val center = CellImageAnalyzer.calcTcrCenter(img.ip.tcr,cell.boundary,Config.tcrCenterAlgorithm)
    	tcr = new Circle(center.x,center.y,cell.boundary.r)
    	cell.tcrCenter = new Point2f(tcr.cx,tcr.cy)
    }else{
      tcr = new Circle(cell.tcrCenter.x,cell.tcrCenter.y,cell.boundary.r)
    }
   	cell.radialTCR = getRadialProfile(img.ip.tcr,cell.boundary,tcr)
   	cell.radialICAM = getRadialProfile(img.ip.icam,cell.boundary,tcr)
    val met = getTcrMetrics(img.ip.tcr,cell.boundary,tcr,cell.radialTCR)
    cell.metricsTCR = new HashMap[String,Float]
   	cell.metricsTCR ++= met
//    println(cell.skewICAM,cell.skewTCR)
  }
  def recalcAllMetrics() {
    cells.foreach(calcCellMetrics(_))
  }
	def readMetricsFromFolder(folder: Path) {
	if(folder.child("celldata.xml").exists)
	  readMetricsFromXmlFile(folder.child("celldata.xml"))
	else if(folder.child("celldata_auto.xml").exists)
	  readMetricsFromXmlFile(folder.child("celldata_auto.xml"))
	else if(folder.child("metrics_mod.txt").exists)
	  readMetricsFromTxtFile(folder.child("metrics_mod.txt"))
	else if(folder.child("metrics.txt").exists)
	  readMetricsFromTxtFile(folder.child("metrics.txt"))
	else
	  throw new Exception("No metrics file.")
	}
	 
	def readMetricsFromXmlFile(m:Path) {
	  observers.foreach(_.cellRemoved(cells.toArray))
	  _cells.clear()
	  metricsFile = m
	  try{
	  val xml = XML.loadFile(m.path)
	  val elems = xml \\ "cell"
	  val cs = elems.par.map(elem=>{
	    val cell = new Cell
	    var num = (elem \ "boundary").text.split(" ").map(_.toFloat)
	    cell.boundary = Circle(num(0),num(1),num(2))
	    num = (elem \ "tcrcenter").text.split(" ").map(_.toFloat)
	    cell.tcrCenter = new Point2f(num(0),num(1))
	    cell.radialTCR = (elem \ "radialtcr").text match{
	      case "" => None
	      case s => Some(s.split(" ").map(_.toDouble).toArray)
	    }
	    cell.radialICAM = (elem \ "radialicam").text match{
	      case "" => None
	      case s => Some(s.split(" ").map(_.toDouble).toArray)
	    }
	    calcCellMetrics(cell)
	    cell
	  }).toArray
	  cs.foreach(c=>_cells += c)
	  println("%d cells were loaded from celldata.xml" format cells.length)
	  }catch{
	  	case e: Exception =>
	  		println("XML parse error (probably due to an empty file)")
	  }
	  observers.foreach(_.cellAdded(cells.toArray))
	}
		
	def readMetricsFromTxtFile(m:Path) {
		metricsFile = m
		observers.foreach(_.cellRemoved(cells.toArray))
		_cells.clear()
	  println("Loading: "+ metricsFile)
		for(line <- Source.fromFile(m.file).getLines){
	   		val token = line.split("\t")
	    		val bfCenter = new Point2f(token(6).toFloat,token(7).toFloat)
	    		var tcrCenter = new Point2f(token(2).toFloat,token(3).toFloat)
	    		val radius = token(8).toFloat
	    		val bf = Circle(bfCenter.x, bfCenter.y, radius)
	    		val tcr = Circle(tcrCenter.x, tcrCenter.y, radius)
	    		val cell = new Cell
	    		cell.boundary = Circle(bfCenter.x,bfCenter.y,radius)
	    		cell.tcrCenter = tcrCenter
	    		calcCellMetrics(cell)
	    		_cells += cell
		}
//		println("%d cells were loaded from %s".format(cells.length,metricsFile.name))
		observers.foreach(_.cellAdded(cells.toArray))
	}
/*	def writeRP() {
		import CellImageAnalyzer.skewness
		var cellcount = 0
		var processcount = 0
		val outradial = metricsFile.parent.child("radialprofile.txt").file
		val pw = new PrintWriter(new BufferedWriter(new FileWriter(outradial)));

		val ip = img.ip
	    var indexcount = 1
	    for(c <- cells){
    		pw.println("%d\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f".format(indexcount,
    		    c.boundary.cx,c.boundary.cy,c.boundary.r,c.tcrCenter.x,c.tcrCenter.y))
    		pw.println(
    		  c.radialTCR match {
    		    case Some(r) => r.mkString("\t")
    		    case None => ""
    		  })
    		pw.println(
    		  c.radialICAM match {
    		    case Some(r) => r.mkString("\t")
    		    case None => ""
    		  })
    		pw.println("Skewness: TCR,ICAM\t%.5f\t%.5f".format(skewness(c.radialTCR),skewness(c.radialICAM)))
    		pw.println
		    indexcount += 1
	    }
		pw.close
	}*/
	def writeMetricsXml(name: String = "celldata.xml") {
	  if(metricsFile==null)
	    return
	  var str = new StringBuilder("<cellmetricsdata>\n")
	  for(c <- cells){
	    str ++= "<cell>\n"
	    str ++= "\t<boundary>%.3f %.3f %.3f</boundary>\n".format(c.boundary.cx,c.boundary.cy,c.boundary.r)
	    str ++= "\t<tcrcenter>%.3f %.3f</tcrcenter>\n".format(c.tcrCenter.x,c.tcrCenter.y)
	    c.radialTCR match {
	      case Some(r) =>
	      	str ++= "\t<radialtcr>" + r.map("%.5f".format(_)).mkString(" ") + "</radialtcr>\n"
	      case None =>
	    }
	    c.radialICAM match {
	      case Some(r) =>
	      	str ++= "\t<radialicam>" + r.map("%.5f".format(_)).mkString(" ") + "</radialicam>\n"
	      case None =>
	    }
	    for(k <- c.metricsTCR.keys.toList.sorted){
	      str ++= "\t<" + k + ">" + c.metricsTCR(k) + "</"+k+">\n"
	    }
	    str ++= "</cell>\n"
	  }
	  str ++= "</cellmetricsdata>\n"
	  val outpath = metricsFile.parent.child(name)
	  val pw = new PrintWriter(outpath.file)
	  pw.print(str)
	  pw.close()
	  println("Output: " + outpath.path)
	}
	def writeMetricsTxt() {
	  if(metricsFile==null){
	    return
	  }
	  val pw = new PrintWriter(metricsFile.parent.child("metrics_mod.txt").file)
	  for(c <- cells){
		  pw.println("%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f".
		      format(1f,0f,c.tcrCenter.x,c.tcrCenter.y,0f,0f,c.boundary.cx,c.boundary.cy,c.boundary.r,0f,0f,0f,0f))
	  }
	  pw.close
	}
	def writeTiledImages() {
	  if(img==null || metricsFile==null)
	    return
	  val width = img.ip.tcr.getWidth
	  val height = img.ip.tcr.getHeight
	  val imtile: BufferedImage = new BufferedImage(width*2,height*2,BufferedImage.TYPE_3BYTE_BGR)
	  val g = imtile.createGraphics
	  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
	  val imadj: Con4[BufferedImage] = new Con4[BufferedImage](img.ip.map(ImgUtils.getAdjustedImage(_)).toArray)
//	  g.drawImage(imadj.bf,null,0,0);		  g.drawImage(imadj.ricm,null,width,0);
//	  g.drawImage(imadj.tcr,null,0,height);		  g.drawImage(imadj.icam,null,width,height);
	  val offset = new Con4[Point](bf = new Point(0,0), ricm = new Point(width,0),
	      tcr = new Point(0,height), icam = new Point(width,height))

	  imadj.zip(offset).foreach({a => g.drawImage(a._1,null,a._2.x,a._2.y)})

	  for(c <- cells){
	    val cir = c.boundary
		g.setStroke(new BasicStroke(2))
		g.setColor(Color.red)
		for(off<-offset){
			g.drawOval(round(cir.cx-cir.r)+off.x,
			    round(cir.cy-cir.r)+off.y,round(cir.r*2),round(cir.r*2))
		}
		g.setColor(Color.blue)
		for(off<-offset){
			val r = 3
			g.fillOval(cir.cxi-r,cir.cyi-r,r*2,r*2)
		}
	  }
	  val outfile: File = metricsFile.parent.child("combined.jpg").file
	  ImageIO.write(imtile, "jpg", outfile)
	  println("Output image: " + outfile.getPath)
	}
	def getImageOfCell(cell: Cell, plane: String, fixedSize: Int = 0, margin: Int = 0): ImageProcessor = {
	  val i = cells.indexOf(cell)
	  if(i == -1)
	    throw new Exception("Cell is not found.")
	  val c = cells(i)
	  var xmin: Int = 0
	  var xmax: Int = 0
	  var ymin: Int = 0
	  var ymax: Int = 0
	  if(fixedSize != 0){
	  	val r = fixedSize/2
	  	xmin = (Array(c.boundary.cx-r,0).max).toInt
	  	xmax = (Array(c.boundary.cx+r,img.width).min).toInt
	  	ymin = (Array(c.boundary.cy-r,0).max).toInt
	  	ymax = (Array(c.boundary.cy+r,img.height).min).toInt
	  }else{
	  	val r = c.boundary.r + margin
	  	xmin = (Array(c.boundary.cx-r,c.tcrCenter.x-r,0).max).toInt
	  	xmax = (Array(c.boundary.cx+r,c.tcrCenter.x+r,img.width).min).toInt
	  	ymin = (Array(c.boundary.cy-r,c.tcrCenter.y-r,0).max).toInt
	  	ymax = (Array(c.boundary.cy+r,c.tcrCenter.y+r,img.height).min).toInt
	  }	  
      val ip = 
    	  plane match {
    	  case "BF" => img.ip.bf
    	  case "RICM" => img.ip.ricm
    	  case "TCR" => img.ip.tcr
    	  case "ICAM" => img.ip.icam
    	  case _ => throw new IllegalArgumentException("getImageOfCell")
	  	}
      ip.setRoi(xmin,ymin,xmax-xmin,ymax-ymin)
      ip.crop
    }
}

trait CellDataObserver {
  def cellRemoved(cells: Array[Cell]): Unit
  def cellAdded(cells: Array[Cell]): Unit
}

class Cell {
	val E = 0.0001f
    var boundary: Circle = _
    var tcrCenter: Point2f = null
    var radialTCR: Option[Array[Double]] = None
    var radialICAM: Option[Array[Double]]= None
    var metricsTCR = new HashMap[String,Float]
    def ==(other: Cell){
	  println("Cell.==() called.")
      abs(boundary.cx - other.boundary.cx) < E &&
      abs(boundary.cy - other.boundary.cy) < E &&
      abs(boundary.r - other.boundary.r) < E
    }
	def formatted: String = {
		"(%.1f, %.1f),r=%.1f com:%.3f skew:%.3f combs:%.3f skewbs:%.3f gini:%.3f ginibs:%.3f".format(boundary.cx,boundary.cy,boundary.r,
	    	metricsTCR("radcom"),metricsTCR("radskew"),metricsTCR("radcomBS"),metricsTCR("radskewBS"),
	    	metricsTCR("radgini"),metricsTCR("radginiBS"))
	}
}