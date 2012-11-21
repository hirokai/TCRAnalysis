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
import hk.Path._
/**
 * New version of Cell data set intended for more flexibility.
 * This is intended for batch calculation, not a GUI manipulation of cells,
 * and contained in DataForOneFlowcell, which is further contained in AllDataMatrix
 * This will keep the calculated information in a folder that is separated from image data.
 *
 */
object CellDataInOneImage {
	/**
	 * This no longer supports .txt input. Only celldata.xml is allowed.
	 * @param metricsFile path to celldata.xml
	 * @return parsed data of cells in one image
	 */
	def newDataSet(metricsFile: Path): Option[CellDataInOneImage] = {
		def nameCheck(m:Path) = "celldata.xml" == m.name
		if(!nameCheck(metricsFile)){
			println(metricsFile)
			throw new Exception("CellDataSet.newDataSet: metrics file name invalid.")
		}
		Config.imgType = 2 //ToDo: Figure out why this is necessary. Analyzer uses this somehow.
		val ds = new CellDataInOneImage
		//Data set is made only when original image exists.
		if (ImageManager.imageExists(metricsFile,Config.imgType)){
			if(ds.readMetricsFromXmlFile(metricsFile))
				Some(ds)
			else
				None
		}else{
			println("No image was found with "+metricsFile.path)
			None
		}
	}
	def createEmptyDataset: CellDataSet = {
		new CellDataSet
	}
}

class CellDataInOneImage {
	var metricsFile: Path = _
	var img: Option[ImageManager] = None
	def loadImage() {
		img = ImageManager.createImageManager(metricsFile,Config.imgType)
	}
	def releaseImage() {
		img.map(_.release())
		img = None
	}

	private var _cells = new ArrayBuffer[Cell]
	def cells = _cells.toArray
	var observers = ArrayBuffer[CellDataObserver]()

	override def finalize() {
		img.map(_.release())
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
		img match {
			case Some(im) => {
				val ip = im.ip
				import CellImageAnalyzer._
				//Assuming boundary input.
				var tcr: Circle = null
				if(calcTcr || cell.tcrCenter==null){
					val center = CellImageAnalyzer.calcTcrCenter(ip.tcr,cell.boundary,Config.tcrCenterAlgorithm)
					tcr = new Circle(center.x,center.y,cell.boundary.r)
					cell.tcrCenter = new Point2f(tcr.cx,tcr.cy)
				}else{
					tcr = new Circle(cell.tcrCenter.x,cell.tcrCenter.y,cell.boundary.r)
				}
				cell.radialTCR = getRadialProfile(ip.tcr,cell.boundary,tcr)
				cell.radialICAM = getRadialProfile(ip.icam,cell.boundary,tcr)
				val met = getTcrMetrics(ip.tcr,cell.boundary,tcr,cell.radialTCR)
				cell.metricsTCR = new HashMap[String,Float]
				cell.metricsTCR ++= met
			}
				case None =>
		}
	}
	def recalcAllMetrics() {
		loadImage()
		cells.foreach(calcCellMetrics(_))
		releaseImage()
	}
	def readMetricsFromFolder(folder: Path) {
		if(folder.child("celldata.xml").exists)
			readMetricsFromXmlFile(folder.child("celldata.xml"))
		else
			throw new Exception("No metrics file.")
	}

	def readMetricsFromXmlFile(m:Path): Boolean = {
		observers.foreach(_.cellRemoved(cells.toArray))
		_cells.clear()
		metricsFile = m

		val xmlStr: String = try{
			val s = Source.fromFile(m.path)
			s.getLines.mkString("\n")
		}catch{
			case _ =>
				""
		}
		if (!xmlStr.isEmpty){
			try{
				loadImage()
				val xml = XML.loadString(xmlStr)
				val elems = xml \\ "cell"
				val cs = elems.map(elem=>{
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
				case e:Exception => {
					println("XML parse error.: ", e.getCause)
					releaseImage()
				}
				return false
			}
		}else{
			println("No celldata.xml found, or celldata.xml is empty.")
			return false
		}
		observers.foreach(_.cellAdded(cells.toArray))
		return true
	}

	def metricsXml: String = {
		var str = new StringBuilder("<cellsinimage>\n")
		str ++= "<imagepath>"
		str ++= "\t%s".format(metricsFile.path)
		str ++= "</imagepath>"
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
		str ++= "</cellsinimage>\n"
		str.toString
	}
	var allMetrics: Array[Double] = _ // Stub

	def writeMetricsXml(outpath: Path) {
		val pw = new PrintWriter(outpath.file)
		pw.print(metricsXml)
		pw.close()
		println("Output: " + outpath.path)
	}

	def writeTiledImages() {
		if(metricsFile==null)
			return
		img match {
			case Some(im) => {
				val ip = im.ip
				val width = ip.tcr.getWidth
				val height = ip.tcr.getHeight
				val imtile: BufferedImage = new BufferedImage(width*2,height*2,BufferedImage.TYPE_3BYTE_BGR)
				val g = imtile.createGraphics
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
				val imadj: Con4[BufferedImage] = new Con4[BufferedImage](ip.map(ImgUtils.getAdjustedImage(_)).toArray)
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
				case None =>
		}


	}
	def getImageOfCell(cell: Cell, plane: String, fixedSize: Int = 0, margin: Int = 0): Option[ImageProcessor] = {
		img match {
			case Some(im) => {
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
					xmax = (Array(c.boundary.cx+r,im.width).min).toInt
					ymin = (Array(c.boundary.cy-r,0).max).toInt
					ymax = (Array(c.boundary.cy+r,im.height).min).toInt
				}else{
					val r = c.boundary.r + margin
					xmin = (Array(c.boundary.cx-r,c.tcrCenter.x-r,0).max).toInt
					xmax = (Array(c.boundary.cx+r,c.tcrCenter.x+r,im.width).min).toInt
					ymin = (Array(c.boundary.cy-r,c.tcrCenter.y-r,0).max).toInt
					ymax = (Array(c.boundary.cy+r,c.tcrCenter.y+r,im.height).min).toInt
				}
				val ip =
					plane match {
						case "BF" => im.ip.bf
						case "RICM" => im.ip.ricm
						case "TCR" => im.ip.tcr
						case "ICAM" => im.ip.icam
						case _ => throw new IllegalArgumentException("getImageOfCell")
					}
				ip.setRoi(xmin,ymin,xmax-xmin,ymax-ymin)
				Some(ip.crop)
			}
			case None =>
				None
		}

	}
}

class Environment {
	var outfolder: Path = _
}

/**
 * A set of images that corresponds to one flowcell (on pat and off pat on 120 nm, 1:1 ag/null).
 * Holds arrays of CellDataInOneImage for aupat and nopat.
 */
class DataForOneFlowcell(env: Environment, x: String, y: String) {
	import DataForOneFlowcell._
	var nopat: Array[CellDataInOneImage] = _
	var aupat: Array[CellDataInOneImage] = _
	def recalcAndSave(){
		recalcAll()
		writeMetricsXml(mkOutputPath(env.outfolder, x,y))
	}
	def recalcAll() {
		nopat.map(_.recalcAllMetrics())
		aupat.map(_.recalcAllMetrics())
	}
	def writeMetricsXml(path: Path) {
		if (path != null){
			println("Output: %s %s: nopat %d images, aupat %d images".format(x,y,nopat.length,aupat.length))
			new PrintWriter(path.file).printf(metricsXml).close()
		}
	}
	def writeMetricsCsv(path: Path) {
		if (path != null){
		val numRows = max(nopat.map(_.cells.length).sum,aupat.map(_.cells.length).sum) + 1
		val keys = CellImageAnalyzer.availableMetrics.keys.toArray.sorted
			val numKeys = keys.length
		val numCols = keys.size * 2
		val vs:Array[Array[String]] = Array.tabulate(numRows,numCols){(r,c) => ""}
		for(i <- 0 to 0; j <- 0 until numKeys){
			vs(i)(j*2) = keys(j) + "_no"
			vs(i)(j*2+1) = keys(j) + "_au"
		}
		val nopatCells = nopat.map(_.cells).flatten
		val aupatCells = aupat.map(_.cells).flatten
		for (i <- 0 until nopatCells.length ;j<-0 until numKeys){
			vs(i+1)(j*2) = nopatCells(i).metricsTCR(keys(j)).toString
		}
		for (i <- 0 until aupatCells.length ;j<-0 until numKeys){
			vs(i+1)(j*2+1) = aupatCells(i).metricsTCR(keys(j)).toString
		}
		val str = vs.map(_.mkString(",")).mkString("\n")
		new PrintWriter(path.file).printf(str).close()
		}
	}
	//ToDo: Use some XML writer to avoid indentation inefficiency.
	def metricsXml: String = {
		val str = new StringBuilder
		str ++= "<flowcell>"
		str ++= "\t<nopat>\n"
		for(i <- nopat.indices){
			str ++= indent(2,nopat(i).metricsXml) + "\n"
		}
		str ++= "\t</nopat>\n\t<aupat>\n"
		for(i <- aupat.indices){
			str ++= indent(2,aupat(i).metricsXml) + "\n"
		}
		str ++= "\t</aupat>\n"
		str ++= "</flowcell>\n"
		str.toString
	}

}

object DataForOneFlowcell {
	def indent(i: Int, str: String): String = str.lines.map("\t"*i + _).mkString("\n")
	def newFromFolder(folder: Path, env: Environment, x:String,y:String): Option[DataForOneFlowcell] = {
		val au: Array[Path] = folder.child("aupat").listFiles("""celldata\.xml$""")
		val no: Array[Path] = folder.child("nopat").listFiles("""celldata\.xml$""")
		val data = new DataForOneFlowcell(env,x,y)
		data.nopat = no.map(CellDataInOneImage.newDataSet(_)).collect{case Some(a) => a}
		data.aupat = au.map(CellDataInOneImage.newDataSet(_)).collect{case Some(a) => a}
		if(data.nopat.length > 0 && data.aupat.length > 0)
			Some(data)
		else
			None
	}
	def mkOutputPath(outfolderBase:Path,x:String,y:String): Path = {
		new Path(escapePath(outfolderBase.path + File.separator + x + "_" + y + ".xml"))
	}
	def mkOutputPathCsv(outfolderBase:Path,x:String,y:String): Path = {
		new Path(escapePath(outfolderBase.path + File.separator + x + "_" + y + ".csv"))
	}
}

/**
 * 2D matrix structure that is made by folder-based image organization.
 * This holds a 2D array of SuperCellDataSet as a substructure.
 */
//ToDo: Read a config file
class AllDataMatrix(configFile: File) {
	import AllDataMatrix._

	val baseFolder = "/Users/hiroyuki/Dropbox/Groves Lab Data/Scope Pics/TCR nanodot all fixed image data set used in the paper";
	var dimX: Array[String] = Array()    // Nanodot Spacing
	var dimY: Array[String] = Array()    // Agonist/null ratio
	var folder: Array[Array[Option[Path]]] = Array.tabulate(5){_=>Array.tabulate(7){_=>None}}

	val environment: Environment = new Environment
	environment.outfolder = new Path(baseFolder + File.separator + "121117 metrics recalculated")

	var data: Array[Array[Option[DataForOneFlowcell]]] = Array.tabulate(5){_=>Array.tabulate(7){_=>None}}
	dimX = Array("40","81","120","145","171")
	dimY = Array("1:0","1:1","1:5","1:10","1:20","1:50","1:100")
	folder = Array.tabulate(5){_=>Array.tabulate(7){_=>None}}
	val datstr =
		"""
81	1:0	20110806_npat/FC01_81nm_1--0_ag--null
81 1:1 20110806_npat/FC02_81nm_1--1_ag--null
81	1:10	20110806_npat/FC04_81nm_1--10_ag--null
120	1:0	20110806_npat/FC05_120nm_1--0_ag--null
120	1:1	20110806_npat/FC06_120nm_1--1_ag--null
120	1:5	20110806_npat/FC07_120nm_1--5_ag--null
120	1:10	20110806_npat/FC08_120nm_1--10_ag--null
145	1:0	20110806_npat/FC09_145nm_1--0_ag--null
145	1:1	20110806_npat/FC10_145nm_1--1_ag--null
145	1:5	20110806_npat/FC11_145nm_1--5_ag--null
81	1:5	20110812_npat_redos/81nm_1--5_ag--null
81	1:20	20110812_npat_redos/81nm_1--20_ag--null
171	1:0	20110812_npat_redos/171nm_1--0_ag--null
171	1:1	20110812_npat_redos/171nm_1--1_ag--null
40	1:0	20111212_npat/FC01_40nm_1--0_ag--null
40	1:1	20111212_npat/FC02_40nm_1--1_ag--null
40	1:5	20111212_npat/FC03_40nm_1--5_ag--null
40	1:10	20111213_npat/FC01_40nm_1--10_ag--null
40	1:20	20111213_npat/FC02_40nm_1--20_ag--null
40	1:50	20111213_npat/FC03_40nm_1--50_ag--null
40	1:100	20111213_npat/FC04_40nm_1--100_ag--null
			""".stripMargin
	datstr.lines.foreach(line=>{
		val vs = line.split("\t")
		if(vs.length == 3) {
			val xi = dimX.indexOf(vs(0))
			val yi = dimY.indexOf(vs(1))
			folder(xi)(yi) = Some(new Path(baseFolder + File.separator + vs(2)))
		}
	})
	def mkString = {
		var str = new StringBuffer
		for(col <- folder){
			for(x <- col){
				str append x.map(_.toString).getOrElse("")
			}
			str.toString
		}
	}
	def getPath(x: String, y: String): Option[Path] = {
		val xi = dimX.indexOf(x)
		val yi = dimY.indexOf(y)
		if(xi>=0 && yi>=0)
			folder(xi)(yi)
		else
			None
	}

	/**
	 * Generates a path for a result file
	 * @param x name of x dimension (=spacing)
	 * @param y name of y dimension (=ag/null ratio)
	 * @return Path for the result file. None if the data does not exist at the specified data point.
	 */
	def getResultFilePath(x: String, y: String): Option[Path] = {
		val xi = dimX.indexOf(x)
		val yi = dimY.indexOf(y)
		if(xi>=0 && yi>=0)
			Some(new Path(baseFolder + File.separator + escapePath(x + "_" + y) + ".xml"))
		else
			None
	}

	def calcAllDataSet {
		for(x <- dimX.indices; y <- dimY.indices){
			folder(x)(y) match {
					case Some(f) => {
						data(x)(y) = DataForOneFlowcell.newFromFolder(f,environment,dimX(x),dimY(y))
					}
					case None =>
						data(x)(y) = None
			}
		}
	}
	def recalcAndSaveAndCloseAllDataSet {
		for(x <- dimX.indices; y <- dimY.indices){
			folder(x)(y) match {
				case Some(f) => {
					data(x)(y) = DataForOneFlowcell.newFromFolder(f,environment,dimX(x),dimY(y))
					data(x)(y) match {
						case Some(d) => {
							d.writeMetricsXml(DataForOneFlowcell.mkOutputPath(environment.outfolder,dimX(x),dimY(y)))
							d.writeMetricsCsv(DataForOneFlowcell.mkOutputPathCsv(environment.outfolder,dimX(x),dimY(y)))
						}
					}
					data(x)(y) = None   //To release memory.
				}
				case None =>
					data(x)(y) = None
			}
		}
	}
	def saveAllDataSet {
		for(x <- dimX.indices; y <- dimY.indices){
			data(x)(y) match {
				case Some(d) => d.recalcAndSave()
				case None =>
			}
		}
	}

	def getDataSet(x: String,y:String): Option[DataForOneFlowcell] = None // Stub
}

object AllDataMatrix {

}
