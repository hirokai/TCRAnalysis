import ij.plugin.frame.PlugInFrame
import ij._
import ij.gui._
import java.awt._
import java.awt.event._
import java.io._
import ij.io.DirectoryChooser
import javax.swing._
import scala.util.Random
import hk._
import javax.swing.event._

object TCR_Analysis {
	val defaultFolder = "/Users/hiroyuki/Documents/"
	val folder = "/Users/hiroyuki/Documents/Groves/TCR/20111027_npat/"
	var instance: TCR_Analysis = _
	//  var frame: TCR_Analysis = _
	//  var _dataSet: CellDataSet = _

	//  def dataSet = _dataSet

	/*  def dataSet_=(ds: CellDataSet): Unit = {
  if (_dataSet != null && _dataSet.img != null)
	_dataSet.img.release
  _dataSet = ds
}*/
}

/**
 * Main class. Entry point for the GUI.
 * @param t window title.
 */

class TCR_Analysis(t: String) extends PlugInFrame(t) with CellDataObserver {
	var leftPanel: JPanel = null
	var pnImage: ImgPanel = _
	var pnDetail: DetailPanel = _
	//  var metrics: Array[Path] = null
	//  var p_currentIndex: Int = 0
	var dataFolder: DataFolder = _
	var dataSet: CellDataSet = _

	//  def currentIndex: Int = p_currentIndex

	/*  def currentIndex_=(v: Int): Unit = {
	if (v == 0)
	  p_currentIndex = 0
	if (metrics == null) return
	if (currentIndex < 0 || currentIndex >= metrics.length) return
	p_currentIndex = v
	if (currentIndex == 0) {
	  btPrev.setEnabled(false)
	  btNext.setEnabled(true)
	} else if (currentIndex == metrics.length - 1) {
	  btPrev.setEnabled(true)
	  btNext.setEnabled(false)
	} else {
	  btPrev.setEnabled(true)
	  btNext.setEnabled(true)
	}
  }
*/
	var btFolder: JButton = _
	var btStack: JButton = _
	var btAuto: JButton = _
	var btReset: JButton = _
	var btPrev: JButton = _
	var btNext: JButton = _
	var btSave: JButton = _
	var cbShuffle: JCheckBox = _
	var lInfoModel: DefaultListModel = _
	var lInfo: JList = _
	var lbStatus: JLabel = _
	var cbAutoSave: JCheckBox = _
	var tfParam: JTextField = _
	var selectedCellIndex: Int = -1

	private val serialVersionUID = 2L

	def this() = {
		this ("TCR analysis")
		println("this()")
		if (TCR_Analysis.instance != null) {
			TCR_Analysis.instance.toFront()
		} else {
			TCR_Analysis.instance = this
			addKeyListener(IJ.getInstance)
		}
		dataSet = CellDataSet.createEmptyDataset
	}

	override def run(arg: String) {
		println("run()")
		prepareGUI()
		setVisible(true)
	}

	def prepareGUI() {
		leftPanel = new JPanel()
		add(leftPanel)
		val layout = new GridBagLayout
		leftPanel.setLayout(layout)
		val gbc = new GridBagConstraints
		gbc.fill = GridBagConstraints.BOTH
		gbc.weightx = 1d
		gbc.weighty = 0d

		//ToDo: Refactor this using GridBagPanel
		btFolder = new JButton("Open (Scope 7)")
		btFolder.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				Config.imgType = 1
				setRootFolder()
			}
		})
		btFolder.setSize(new Dimension(200, 50))
		gbc.gridx = 0
		gbc.gridy = 0
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(btFolder, gbc)

		btStack = new JButton("Open (Scope 2)")
		btStack.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				Config.imgType = 2
				setRootFolder()
			}
		})
		btStack.setSize(new Dimension(200, 50))
		gbc.gridx = 3
		gbc.gridy = 0
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(btStack, gbc)

		btAuto = new JButton("Start auto calc")
		btAuto.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {doAutoCalc()}
		})
		btAuto.setSize(new Dimension(200, 50))
		gbc.gridx = 0
		gbc.gridy = 1
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(btAuto, gbc)

		val btCompile = new JButton("Compile files")
		btCompile.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {compileData()}
		})
		btCompile.setSize(new Dimension(200, 50))
		gbc.gridx = 3
		gbc.gridy = 1
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(btCompile, gbc)

		btPrev = new JButton("Prev")
		btPrev.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {moveFileIndex(-1)}
		})
		btPrev.setSize(new Dimension(100, 50))
		gbc.gridx = 0
		gbc.gridy = 2
		gbc.gridwidth = 2
		gbc.gridheight = 1
		leftPanel.add(btPrev, gbc)

		btNext = new JButton("Next")
		btNext.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {moveFileIndex(1)}
		})
		btNext.setSize(new Dimension(100, 50))
		gbc.gridx = 4
		gbc.gridy = 2
		gbc.gridwidth = 2
		gbc.gridheight = 1
		leftPanel.add(btNext, gbc)

		btSave = new JButton("Save")
		btSave.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				dataSet.recalcAllMetrics()
				dataSet.writeMetricsXml()
			}
		})
		btSave.setSize(new Dimension(100, 50))
		gbc.gridx = 0
		gbc.gridy = 3
		gbc.gridwidth = 3
		gbc.gridheight = 1
		//		btSave.setEnabled(!Config.autoSave)
		leftPanel.add(btSave, gbc)

		cbAutoSave = new JCheckBox("Autosave")
		cbAutoSave.addItemListener(new ItemListener() {
			def itemStateChanged(e: ItemEvent) {
				Config.autoSave = cbAutoSave.isSelected
				//			  btSave.setEnabled(!Config.autoSave)
			}
		})
		cbAutoSave.setSelected(Config.autoSave)
		cbAutoSave.setSize(new Dimension(100, 50))
		gbc.gridx = 3
		gbc.gridy = 3
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(cbAutoSave, gbc)

		val btTest = new JButton("Test")
		btTest.addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {testAlgorithm()}
		})
		btTest.setSize(new Dimension(100, 50))
		gbc.gridx = 0
		gbc.gridy = 4
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(btTest, gbc)

		tfParam = new JTextField
		tfParam.setSize(new Dimension(100, 50))
		gbc.gridx = 3
		gbc.gridy = 4
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(tfParam, gbc)

		val chAlgorithm = new JComboBox
		TcrAlgorithm.name.values.map(chAlgorithm.addItem(_))
		chAlgorithm.setSelectedItem(TcrAlgorithm.name(Config.tcrCenterAlgorithm))
		chAlgorithm.addItemListener(new ItemListener() {
			def itemStateChanged(e: ItemEvent) {
				Config.tcrCenterAlgorithm = chAlgorithm.getSelectedIndex match {
					case 0 => TcrAlgorithm.CoM
					case 1 => TcrAlgorithm.CoM3
					case 2 => TcrAlgorithm.CoMsubtractBG
					case 3 => TcrAlgorithm.CoM3subtractBG
				}
				dataSet.recalcAllMetrics()
			}
		})
		chAlgorithm.setSize(new Dimension(100, 50))
		gbc.gridx = 3
		gbc.gridy = 5
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(chAlgorithm, gbc)

		val chPickCells = new JComboBox
		CellPickMethod.name.values.map(chPickCells.addItem(_))
		chPickCells.setSelectedItem(CellPickMethod.name(Config.cellPickMethod))
		chPickCells.addItemListener(new ItemListener() {
			def itemStateChanged(e: ItemEvent) {
				Config.cellPickMethod = chPickCells.getSelectedIndex match {
					case 0 => CellPickMethod.CircleOrigin
					case 1 => CellPickMethod.CircleRect
				}
			}
		})
		chPickCells.setSize(new Dimension(100, 50))
		gbc.gridx = 0
		gbc.gridy = 5
		gbc.gridwidth = 3
		gbc.gridheight = 1
		leftPanel.add(chPickCells, gbc)

		lInfoModel = new DefaultListModel
		lInfo = new JList(lInfoModel)
		lInfo.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
		lInfo.setSize(new Dimension(200, 300))
		lInfo.addListSelectionListener(new ListSelectionListener {
			def valueChanged(e: ListSelectionEvent) {
				val index = lInfo.getSelectedIndex
				doAction("SelectCell", index)
			}
		})
		val spInfo = new JScrollPane(lInfo, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)

		gbc.gridx = 0
		gbc.gridy = 6
		gbc.gridwidth = 6
		gbc.gridheight = 3
		gbc.weighty = 4d
		leftPanel.add(spInfo, gbc)
		pnDetail = new DetailPanel(this)
		gbc.gridx = 0
		gbc.gridy = 12
		gbc.gridwidth = 6
		gbc.gridheight = 8
		gbc.weighty = 4d
		leftPanel.add(pnDetail, gbc)


		lbStatus = new JLabel
		lbStatus.setSize(new Dimension(50, 300))
		gbc.gridx = 0
		gbc.gridy = 18
		gbc.gridwidth = 6
		gbc.gridheight = 1
		gbc.weighty = 0d
		leftPanel.add(lbStatus, gbc)

		pnImage = new ImgPanel(this)
		pnImage.init

		val leftBigPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,leftPanel,pnDetail)
		leftBigPanel.setContinuousLayout(true)

		val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftBigPanel, pnImage)
		splitPane.setContinuousLayout(true)
		add(splitPane)

		addWindowListener(new WindowAdapter() {
			override def windowClosed(e: WindowEvent) {
				dispose()
				if (Config.autoSave) {
					dataSet.recalcAllMetrics()
					dataSet.writeMetricsXml()
				}
				dataSet = null
				dataFolder = null
			}
		})
		splitPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0),"nextFile")
		splitPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0),"prevFile")
		splitPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, 0),"removeCell")
		splitPane.getActionMap.put("nextFile",moveNextFileAction)
		splitPane.getActionMap.put("prevFile",movePrevFileAction)
		splitPane.getActionMap.put("removeCell",removeSelectedCellAction)

		setPreferredSize(new Dimension(1000, 700))
		setExtendedState(getExtendedState | Frame.MAXIMIZED_BOTH)
		pack()
	}

	val moveNextFileAction = new AbstractAction() {
		def actionPerformed(p1: ActionEvent) {
			moveFileIndex(1)
		}
	}

	val movePrevFileAction = new AbstractAction() {
		def actionPerformed(p1: ActionEvent) {
			moveFileIndex(-1)
		}
	}
	
	val removeSelectedCellAction = new AbstractAction() {
		def actionPerformed(p1: ActionEvent) {
			val i = selectedCellIndex
			if (i != -1) {
				val c = dataSet.cells(i)
				dataSet.removeCell(Array(c))
				repaint()
			}
		}
	}


	val keyAdapter = new KeyAdapter() {
		override def keyPressed(e: KeyEvent) {
			println("keyPressed()")
			e.getKeyCode match {
				case KeyEvent.VK_RIGHT =>
					moveFileIndex(1)
				case KeyEvent.VK_LEFT =>
					moveFileIndex(-1)
				case KeyEvent.VK_UP =>
				case KeyEvent.VK_DOWN =>
				case KeyEvent.VK_BACK_SPACE =>
					println("back space key")
				case _ =>
			}
		}
	}

	def setUIEnabled(b: Boolean) {
		if (b) {
			Array(btFolder, btStack, btAuto, lInfo).foreach(_.setEnabled(true))
			btPrev.setEnabled(dataFolder.currentIndex > 0)
			btNext.setEnabled(dataFolder.currentIndex < dataFolder.fileCount - 1)
			btSave.setEnabled(true)
			cbAutoSave.setEnabled(true)
		} else {
			Array(btPrev, btNext, btFolder, btStack, btAuto,
				btSave, cbAutoSave, lInfo).foreach(_.setEnabled(false))
		}
	}

	def moveFileIndex(diff: Int) {
		if (dataFolder.currentIndex + diff >= dataFolder.fileCount || dataFolder.currentIndex + diff < 0)
			return
		setUIEnabled(false)
		if (Config.autoSave) {
			dataSet.recalcAllMetrics()
			dataSet.writeMetricsXml()
		}
		dataFolder.currentIndex += diff
		selectedCellIndex = -1
		dataFolder.getCurrentDataSet.observers += this
		dataSet = dataFolder.getCurrentDataSet
		cellListViewUpdate()
		setUIEnabled(true)
		pnImage.dataChanged(dataFolder,dataSet)
		pnDetail.dataChanged(dataFolder,dataSet)
	}

	def cellListViewUpdate() {
		if(dataSet!=null){
			lInfoModel.removeAllElements()
			dataSet.cells.foreach(d => {
				lInfoModel.addElement(d.formatted)
			})
		}
	}

	def selectCellMove(diff: Int) {
		if (selectedCellIndex + diff >= dataSet.cells.length || selectedCellIndex + diff < 0)
			return
		selectedCellIndex += diff
		doAction("SelectCell",selectedCellIndex)
		lInfo.setSelectedIndex(selectedCellIndex)
	}

	def setRootFolder() {
		DirectoryChooser.setDefaultDirectory(TCR_Analysis.defaultFolder)
		val folder: String = (new DirectoryChooser("Choose a target folder of analysis")).getDirectory
		println(folder)
		if (folder == null)
			return
		var ds: CellDataSet = null
		if (Config.autoSave && dataFolder != null && (ds = dataFolder.getCurrentDataSet) != null) {
			ds.recalcAllMetrics()
			ds.writeMetricsXml()
		}
		val metrics = DataFolder.searchAndMakeMetrics(folder)
		if (metrics.length == 0) {
			IJ.error("TCR analysis", "No celldata.xml files found.")
			return
		}
		if (Config.shuffleMode) shuffleArray(metrics)
		dataFolder = new DataFolder(metrics).init
		dataFolder.currentIndex = 0
		dataSet = dataFolder.getCurrentDataSet
		dataSet.observers += this
		cellListViewUpdate()
		pnImage.dataChanged(dataFolder,dataSet)
		pnDetail.dataChanged(dataFolder,dataSet)
	}

	def doAutoCalc() {
		var allfilecount = 0
		// var allcellcount = 0
		val proceed = IJ.showMessageWithCancel("TCR analysis",
			"%d files will be processed. It may take a long time".format(dataFolder.fileCount))
		if (!proceed)
			return
		Config.batchMode = true
		val numFiles = dataFolder.fileCount
		for(i <- 0 until numFiles){
			try {
				dataFolder.currentIndex = i
				val ds = dataFolder.getCurrentDataSet
				ds.writeMetricsXml("celldata.xml")
				allfilecount += 1
			} catch {
				case e: StackFileIncorrectException =>
					println("Stk file was incorrect. skipped: " + dataFolder.metrics(i).path)
				case _ =>
			}
			if (allfilecount % 10 == 0 && allfilecount > 0) {
				println("%d/%d files processed.".format(allfilecount, numFiles))
			}
			lbStatus.setText("%d/%d files processed.".format(allfilecount, numFiles))
		}
		Config.batchMode = false
		lbStatus.setText("Auto calc done.")
	}

	def testAlgorithm() {
		if (selectedCellIndex == -1)
			return
		val cell = dataSet.cells(selectedCellIndex)
		val newip = dataSet.getImageOfCell(cell, "TCR")
		try {
			TestAnalysis3.run(newip, tfParam.getText)
		} catch {
			case e: Exception => IJ.error(e.getClass.getName, e.getMessage)
			e.printStackTrace()
		}
	}

	def compileData() {
		DirectoryChooser.setDefaultDirectory(TCR_Analysis.defaultFolder)
		val folder: String = (new DirectoryChooser("Choose a target folder of analysis")).getDirectory
		if (folder != null) {
			val xml: Path = new Path(folder + File.separator + "alldata.xml")
			val csvtcr: Path = new Path(folder + File.separator + "radial_tcr.csv")
			val csvicam: Path = new Path(folder + File.separator + "radial_icam.csv")
			DataFolder.compileDataFolder(new Path(folder), xml, csvtcr, csvicam)
		}
	}

	def doAction(command: String, paramInt: Int) {
		println("doAction(): %s, %d".format(command, paramInt))
		command match {
			case "SelectCell" =>
				selectedCellIndex = paramInt
				pnDetail.selectCell(selectedCellIndex)
				repaint()
			case "ShowCellInfo" =>
				selectedCellIndex = paramInt
				if (selectedCellIndex >= 0)
					showCellInfo(dataSet.cells(selectedCellIndex))
			case _ =>
		}
	}

	def showCellInfo(cell: Cell)() {
		val dialog = new JFrame("Cell info")
		val keys = cell.metricsTCR.keys.toArray.sorted
		val str = keys.map(k => k + "\t\t" + cell.metricsTCR(k)).mkString("\n")
		val ta = new JTextArea(str)
		ta.setWrapStyleWord(true)
		dialog.add(ta)
		val pane = dialog.getContentPane
		pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS))
		val tfSize = new JTextField("250")
		dialog.add(tfSize)
		val btImg = new JButton("Output image")
		btImg.addActionListener(new ActionListener {
			def actionPerformed(e: ActionEvent) {
				val size = tfSize.getText.toInt
				val ipBf = dataSet.getImageOfCell(cell, "BF", size)
				val f4 = new ImageWindow(new ImagePlus("BF", ipBf))
				f4.setLocation(400, 200)
				f4.setVisible(true)
				val ipRicm = dataSet.getImageOfCell(cell, "RICM", size)
				val f3 = new ImageWindow(new ImagePlus("RICM", ipRicm))
				f3.setLocation(400 + size + 10, 200)
				f3.setVisible(true)
				val ipTcr = dataSet.getImageOfCell(cell, "TCR", size)
				val f1 = new ImageWindow(new ImagePlus("TCR", ipTcr))
				f1.setLocation(400, 200 + size + 30)
				f1.setVisible(true)
				val ipIcam = dataSet.getImageOfCell(cell, "ICAM", size)
				val f2 = new ImageWindow(new ImagePlus("ICAM", ipIcam))
				f2.setLocation(400 + size + 10, 200 + size + 30)
				f2.setVisible(true)
			}
		})
		dialog.add(btImg)
		dialog.setSize(300, 200)
		dialog.pack()
		dialog.setVisible(true)
	}

	def cellRemoved(cells: Array[Cell]) {
		//    println("TCR_Analysis.cellRemoved()")
		selectedCellIndex = -1
		cellListViewUpdate()
		pnImage.repaint(null)
	}

	def cellAdded(cells: Array[Cell]) {
		println("cellAdded(): %d.cells.".format(cells.length))
		cells.foreach(c =>
			lInfoModel.addElement(c.formatted)
		)
		pnImage.repaint(null)
	}

	def shuffleArray[A](a: Array[A]) {
		val len = a.length
		for (i <- 0 until len - 1) {
			val j = Random.nextInt(len - i - 1) + i + 1
			swap(a, i, j)
		}
		def swap(a: Array[A], i: Int, j: Int) {
			val temp: A = a(i)
			a(i) = a(j)
			a(j) = temp
		}
	}
}

