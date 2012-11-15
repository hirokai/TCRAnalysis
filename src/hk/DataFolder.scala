package hk

import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.File
import scala.io.Source
import scala.xml.XML

object DataFolder {
	def searchAndMakeMetrics(folder: String): Array[Path] = {
		val exists = (new FileSearch).listFiles(folder, "celldata\\.xml").map(Path(_))
		val made = new ArrayBuffer[Path]
		Config.imgType match {
			case 1 => {
				//Scope 7
				println(folder)
				val tiffs = (new FileSearch).listFiles(folder, Config.filename.bf).map(Path(_))
				println(tiffs.length)
				for (tiff <- tiffs) {
					//create folder analysis_**
					val name = "analysis"
					val f = tiff.parent.child(name)
					if (!f.file.exists) {
						f.file.mkdir
						val m = f.child("celldata.xml")
						if (!m.exists) {
							m.file.createNewFile
							made += m
						}
					}
				}
			}
			case 2 => {
				println(folder)
				val stacks = (new FileSearch).listFiles(folder, """.+\.stk$""").map(Path(_))
				println(stacks.length)
				for (stk <- stacks) {
					//create folder analysis_**
					val number = """(.+)\.stk""".r.findFirstMatchIn(stk.file.getName).get
					val name = "analysis_" + number.subgroups(0)
					val f = stk.parent.child(name)
					if (!f.file.exists) {
						f.file.mkdir
						val m = f.child("celldata.xml")
						if (!m.exists) {
							m.file.createNewFile
							made += m
						}
					}
				}
			}
		}
		exists ++ made
	}

	def compileDataFolder(folder: Path, xml: Path, csvtcr: Path, csvicam: Path): Unit = {
		val outxml = new PrintWriter(xml.file);
		val outtcr = new PrintWriter(csvtcr.file);
		val outicam = new PrintWriter(csvicam.file);
		if (folder == null)
			return
		outxml.println("<celldatacompilation>")
		var count = 0;
		search(folder.file)
		outxml.println("</celldatacompilation>")
		outxml.close
		outtcr.close
		outicam.close
		def search(folder: File): Unit = {
			val files = folder.listFiles
			for (f <- files) {
				//			  println(f.getPath)
				val path = f.getPath
				if (f.isDirectory) search(f)
				else if (path.contains("celldata.xml")) {
					outxml.println("<datafile>")
					outxml.println("<filename>" + f.getPath + "</filename>")
					val s = Source.fromFile(f)
					s.getLines.foreach(outxml.println(_))
					outxml.println("</datafile>")
					try {
						val xml = XML.load(f.toURL);
						for (tcr <- xml \\ "radialtcr") {
							outtcr.println(tcr.text.split("\\s+").mkString(","));
						}
						for (icam <- xml \\ "radialicam") {
							outicam.println(icam.text.split("\\s+").mkString(","));
						}
					} catch {
						case e: Exception => println(e.getMessage());
					}
					count += 1;
					Utils.printStatus("%d files processed.".format(count));
				}
			}
		}
	}

}

class DataFolder(var metrics: Array[Path]) {
	def init: DataFolder = {
		val len = metrics.length
		for(i <- 0 until len){
			dataSet(i) = CellDataSet.newDataSet(metrics(i))
		}
		this
	}

	val dataSet = new Array[CellDataSet](metrics.length)
	def fileCount: Int = {
		metrics.length
	}
	def getCurrentDataSet: CellDataSet = {
		dataSet(currentIndex)
	}
	var _currentIndex: Int = -1
	def currentIndex: Int = _currentIndex
	def currentIndex_=(index: Int): Int = {
		if (currentIndex != index) {
			_currentIndex = index
			var ds = dataSet(index)
			if(ds==null){
				ds = CellDataSet.newDataSet(metrics(currentIndex))
				dataSet(index) = ds
			}
		}
		index
	}
	def getAllValuesInFolder(keys: Array[String]): Array[Array[Double]] = {
		val cs = new ArrayBuffer[Cell]
		dataSet.foreach(d=>{
			if(d!=null) cs ++= d.cells
		})
		cs.map(c=>{
			keys.map(k=>{
			c.metricsTCR(k).toDouble
			})
		}).toArray
	}
}