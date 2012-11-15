package hk

import java.awt._
import java.awt.event._
import java.awt.geom.Ellipse2D
import javax.swing._
import org.jfree.chart._
import axis._
import org.jfree.chart.plot._
import org.jfree.data.xy.DefaultXYDataset
import hk._
import renderer.category.ScatterRenderer
import renderer.xy._


class DetailPanel extends JPanel {
	var chartPanel: ChartPanel = _
	var nameA = "X axis"
	var nameB = "Y axis"
	var keyA: Option[String] = Some("radcom")
	var keyB: Option[String] = Some("radskew")
	val metricname: Map[String, String] = RadialProfileAnalyzer.availableMetrics
	val metrickey: Map[String, String] = metricname.map(a => (a._2, a._1))
	var metricsData = new DefaultXYDataset
	var dataSet: CellDataSet = _
	var dataFolder: DataFolder = _
	var selectedIndex: Int = -1
	var chart: JFreeChart = _
	var plot: Plot = _
	var xAxis: NumberAxis = _
	var yAxis: NumberAxis = _
	var populationData: Array[Array[Double]] = _
	var selectedData: Array[Array[Double]] = _
	val renderer = new XYLineAndShapeRenderer(false, true)

	def this(parentFrame: Frame) = {
		this ()
		setBackground(new Color(230, 230, 255));
		renderer.setSeriesFillPaint(0, Color.blue)
		renderer.setSeriesShape(0, new Ellipse2D.Double(-1, -1, 2, 2), true)
		xAxis = new NumberAxis(nameA)
		yAxis = new NumberAxis(nameB)
		xAxis.setAutoRange(true);
		yAxis.setAutoRange(true);
		xAxis.setAutoRangeIncludesZero(false)
		yAxis.setAutoRangeIncludesZero(false)
		plot = new XYPlot(metricsData, xAxis, yAxis, renderer)
		chart = new JFreeChart("Metrics", plot)
		chartPanel = new ChartPanel(chart, true)
		addComponentListener(new ComponentAdapter() {
			override def componentResized(e: ComponentEvent) {
				val size = getSize()
				chartPanel.setMaximumSize(size)
				chartPanel.setSize(size)
			}
		})
		val layout = new GridBagLayout
		setLayout(layout)
		val gbc = new GridBagConstraints;
		val lbAxisX = new JLabel("X axis");
		val cbAxisX = new JComboBox
		metrickey.foreach(a => cbAxisX.addItem(a._1))
		cbAxisX.addItemListener(new ItemListener {
			def itemStateChanged(p1: ItemEvent) {
				val name = cbAxisX.getSelectedItem.asInstanceOf[String]
				val key = metrickey(name)
				selectKeys(key, keyB.get);
			}
		})
		cbAxisX.setSelectedItem(metricname(keyA.get))
		val lbAxisY = new JLabel("Y axis");
		val cbAxisY = new JComboBox
		metrickey.foreach(a => cbAxisY.addItem(a._1))
		cbAxisY.addItemListener(new ItemListener {
			def itemStateChanged(p1: ItemEvent) {
				val name = cbAxisY.getSelectedItem.asInstanceOf[String]
				val key = metrickey(name)
				selectKeys(keyA.get, key);
			}
		})
		cbAxisY.setSelectedItem(metricname(keyB.get))
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 0;
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		add(lbAxisX, gbc)
		gbc.gridx = 1;
		add(cbAxisX, gbc)
		gbc.gridx = 2;
		add(lbAxisY, gbc)
		gbc.gridx = 3;
		add(cbAxisY, gbc)
		gbc.weighty = 1;
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 4;
		gbc.gridheight = 4;
		add(chartPanel, gbc)
	}

	def setPopulationData(d: Array[Array[Double]]): Unit = {
		populationData = d
		metricsData.removeSeries("population data")
		metricsData.addSeries("population data", populationData)
	}

	def setSelectedData(d: Array[Array[Double]]): Unit = {
		selectedData = d
		println("setSelectedData(): length: %d".format(d.length))
		metricsData.removeSeries("selected data")
		metricsData.addSeries("selected data", selectedData)
		//		val i1 = metricsData.indexOf("population data")
		val i2 = metricsData.indexOf("selected data")
		//		renderer.setSeriesFillPaint(i1,Color.blue)
		//		renderer.setSeriesShape(i1,new Ellipse2D.Double(-1,-1,2,2),true)
		renderer.setSeriesFillPaint(i2, Color.red)
		renderer.setSeriesShape(i2, new Ellipse2D.Double(-3, -3, 6, 6), true)
	}

	def testAddData {
		setPopulationData(Array(Array(1, 2, 3, 4), Array(2, 4, 6, 7)))
	}

	def dataChanged(df: DataFolder, ds: CellDataSet) {
		var recalc = false
		if (dataFolder != df) {
			dataFolder = df
			selectedIndex = -1
			recalc = true;
		}
		if (dataSet != ds) {
			selectedIndex = -1
			dataSet = ds
			recalc = true;
		}
		if (recalc) recalcAll
	}

	def recalcAll {
		(keyA, keyB) match {
			case (Some(ka), Some(kb)) => {
				if (selectedIndex != -1) {
					val c = dataSet.cells(selectedIndex)
					setSelectedData(Array(Array(c.metricsTCR(ka)), Array(c.metricsTCR(kb))))
					val i2 = metricsData.indexOf("selected data")
					renderer.setSeriesFillPaint(i2, Color.red)
					renderer.setSeriesShape(i2, new Ellipse2D.Double(-3, -3, 6, 6), true)
				}
				if (dataFolder != null) {
					val metrics: Array[Array[Double]] = dataFolder.getAllValuesInFolder(Array(ka, kb))
					setPopulationData(metrics.transpose)
					val i1 = metricsData.indexOf("population data")
					renderer.setSeriesFillPaint(i1, Color.blue)
					renderer.setSeriesShape(i1, new Ellipse2D.Double(-1, -1, 2, 2), true)
				}}
			case _ =>
		}
	}

	def selectCell(index: Int) {
		selectedIndex = index
		if (selectedIndex != -1) {
			val c = dataSet.cells(selectedIndex)
			(keyA, keyB) match {
				case (Some(ka), Some(kb)) => {
					setSelectedData(Array(Array(c.metricsTCR(ka)), Array(c.metricsTCR(kb))))
				}
				case _ =>
			}
		}
	}

	def selectKeys(k1: String, k2: String) {
		keyA = Some(k1);
		keyB = Some(k2);
		nameA = metricname(k1);
		nameB = metricname(k2)
		xAxis.setLabel(nameA)
		yAxis.setLabel(nameB)
		recalcAll
	}
}