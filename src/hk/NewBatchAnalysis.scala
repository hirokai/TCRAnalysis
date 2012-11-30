package hk

import javax.swing.JFrame
import ij.gui.{StackWindow, ImageWindow}
import ij.{ImageStack, ImagePlus}
import ij.plugin.CanvasResizer

/**
 * Created with IntelliJ IDEA.
 * User: hiroyuki
 * Date: 11/15/12
 * Time: 6:48 PM
 * To change this template use File | Settings | File Templates.
 */
object NewBatchAnalysis {
	def run(parent: JFrame) {
		val data = new AllDataMatrix(null) //ToDo: pass a config file
		data.recalcAndSaveAndCloseAllDataSet
	}
}

class ShowcaseMetrics {
	def run(parent: JFrame) {
		val matrix = new AllDataMatrix(null)
		// val metricsKeys = CellImageAnalyzer.availableMetrics.keys

		//Data set is fixed right now.
		matrix.getDataForFlowcell("81","1:0") match {
			case Some(flowcell) => {
				//This loads all images in one flowcell, so this should not be used for the entire data set,
				// which will be too large.
				val allcells: Array[(Cell,CellDataInOneImage)]
					= (flowcell.aupat ++ flowcell.nopat).map(a => {
						println("Loading...")
						a.loadImage()
						a.cells.map(c => (c,a))
					}).flatten.toArray

				val keys = CellImageAnalyzer.availableMetrics.keys
				// val keys = Array("radskew","radvarMirror","moment2nd")
				for(key <- keys){
					val cim: Iterable[(Cell,CellDataInOneImage)]
						= allcells.filter(_._1.metricsTCR(key) != Float.NaN).sortBy(_._1.metricsTCR(key))
					val stk = new ImageStack(200,200)
					for (c <- cim) {
						val cell = c._1
						val im = c._2
						var Some(img) = im.getImageOfCell(cell,"TCR",200)
						img.resetMinAndMax()
						if (img.getWidth < 200 || img.getHeight < 200)
							img = (new CanvasResizer).expandImage(img,200,200,0,0)
						stk.addSlice("%.3e".format(c._1.metricsTCR(key)),img)
					}
					val f1 = new StackWindow(new ImagePlus(CellImageAnalyzer.availableMetrics(key),stk))
					f1.setLocation(400, 200)
					f1.setVisible(true)
				}
			}
		}
	}
}
