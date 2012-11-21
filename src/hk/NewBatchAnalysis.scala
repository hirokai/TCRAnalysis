package hk

import javax.swing.JFrame

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
