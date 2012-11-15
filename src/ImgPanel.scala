import ij.process.ImageProcessor
import java.awt._
import java.awt.event.KeyEvent
import java.awt.event.KeyAdapter
import scala.math._
import java.awt.geom.AffineTransform
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.geom.Point2D
import javax.swing._;
import java.awt.event.MouseMotionAdapter
import hk._;

class ImgPanel extends JPanel with MouseListener {
	var active: Boolean = false;
	var parentFrame: TCR_Analysis = _
	var dataSet: CellDataSet = _
	var dataFolder: DataFolder = _
	var images: Array[Image] = _
	var window, draw, original, margin: Dimension = _
	var scale: Float = _
	var dashStroke: BasicStroke = _
	val basicStroke = new BasicStroke(3)
	val trans = new Con4[AffineTransform]
	var notrans: AffineTransform = null
	var invtrans = new Con4[AffineTransform]

	var manager: ImageManager = _

	var ip: Con4[ImageProcessor] = _
	var im: Con4[Image] = _

	def this(_parent: TCR_Analysis) {
		this ()
		parentFrame = _parent;

		val dash1 = Array(10f, 10f)
		dashStroke = new BasicStroke(4f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER,
			3.0f, dash1, 0.0f);
		window = new Dimension();
		draw = new Dimension();
		original = new Dimension();
		println(original)
		margin = new Dimension();
		val scrSize = Toolkit.getDefaultToolkit.getScreenSize
		window.width = (scrSize.width * 0.6).toInt;
		window.height = (scrSize.height * 0.8).toInt;
		margin.width = 0;
		margin.height = 0;
		setBackground(new Color(200, 200, 255))
	}

	def dataChanged(df: DataFolder, ds: CellDataSet): Unit = {
		println("ImgPanel.dataChanged()")
		if(dataSet!=null)
			dataSet.img.release
		dataSet = ds
		dataFolder = df
		if (df != null && ds != null)
			active = true;
		else{
			active = false;
			return
		}
		//    val doneStr = if (ds.metricsFile.name == "celldata.xml") " (revisit)" else " (new)"
		val doneStr = ""
		var titleStr: String = ""
		if (Config.shuffleMode)
			titleStr = "(#%d/%d)%s".format(df.currentIndex + 1, df.fileCount, doneStr)
		else
			titleStr = "(#%d/%d) %s%s".format(df.currentIndex + 1, df.fileCount, ds.img.identityPath, doneStr)
		parentFrame.setTitle(titleStr)
		ip = ds.img.ip
		im = new Con4[Image](ip.map(ImgUtils.getAdjustedImage(_)).toArray)
		original.width = ip.bf.getWidth
		original.height = ip.bf.getHeight
		this.repaint()
	}

	def calcDimension: Unit = {
		window = this.getSize
		if (window.width.toFloat / window.height >
			original.width.toFloat / original.height) {
			scale = window.height.toFloat / original.height / 2
		} else {
			scale = window.width.toFloat / original.width / 2
		}
		draw.height = round(original.height * scale)
		draw.width = round(original.width * scale)
		val off_x = 0;
		trans.bf = new AffineTransform(scale, 0, 0, scale, off_x, 0);
		trans.ricm = new AffineTransform(scale, 0, 0, scale, off_x + draw.width, 0);
		trans.tcr = new AffineTransform(scale, 0, 0, scale, off_x, draw.height);
		trans.icam = new AffineTransform(scale, 0, 0, scale, off_x + draw.width, draw.height);
		invtrans = new Con4[AffineTransform](trans.map(_.createInverse).toArray)
	}

	var dragOrigin: Point = null
	var dragPoint: Point = null

	val motion = new MouseMotionAdapter() {
		override def mouseDragged(e: MouseEvent) {
			Config.cellPickMethod match {
				case CellPickMethod.CircleOrigin => {
					dragPoint.x = e.getX
					dragPoint.y = e.getY
				}
				case CellPickMethod.CircleRect => {
					dragPoint.x = e.getX
					dragPoint.y = e.getY
					println(dragPoint)
					val side = min(abs(dragPoint.x - dragOrigin.x), abs(dragPoint.y - dragOrigin.y))
					dragPoint.x = dragOrigin.x + side * signum(dragPoint.x - dragOrigin.x)
					dragPoint.y = dragOrigin.y + side * signum(dragPoint.y - dragOrigin.y)
					println(dragPoint)
				}
			}
			repaint()
		}
	}

	override def mousePressed(e: MouseEvent) = {
		if (e.getButton == MouseEvent.BUTTON3) {
			val cs = findCellsFromPosition(new Point2D.Float(e.getX, e.getY))
			dataSet.removeCell(cs)
		} else if (e.getButton == MouseEvent.BUTTON1) {
			dragOrigin = new Point(e.getX, e.getY)
			if(isInsideImage(dragOrigin)){
				dragPoint = new Point(e.getX, e.getY)
				addMouseMotionListener(motion)
			}else
				dragOrigin = null
		}
	}

	def isInsideImage(p: Point2D.Float): Boolean = {
		println("isInsideImage(): "+p.toString)
		p.getX < draw.width * 2 || p.getY < draw.height * 2
	}

	override def mouseExited(e: MouseEvent): Unit = {}

	override def mouseEntered(e: MouseEvent): Unit = {}

	override def mouseClicked(e: MouseEvent): Unit = {}

	override def mouseReleased(e: MouseEvent): Unit = {
		if (e.getButton == MouseEvent.BUTTON1) {
			if (dragPoint.distance(dragOrigin) < 2) {
				val cs = findCellsFromPosition(new Point2D.Float(e.getX, e.getY))
				val c = if (cs.length > 0) cs(0) else return
				val index: Int = getIndexOfCell(c)
				println(e.getClickCount)
				if (e.getClickCount > 1)
					parentFrame.doAction("ShowCellInfo", index)
				else
					parentFrame.doAction("SelectCell", index)
				return
			}
			calcDimension
			removeMouseMotionListener(motion)
			val affine: AffineTransform = judgeAndGetATOfPoint(dragOrigin)
			Config.cellPickMethod match {
				case CellPickMethod.CircleOrigin => {
					val origin = new Point2D.Float
					val periphery = new Point2D.Float
					affine.transform(dragOrigin, origin)
					affine.transform(dragPoint, periphery)
					if (origin.distance(periphery) > 1) {
						dataSet.addCell(origin.x, origin.y, origin.distance(periphery).toFloat)
					}
				}
				case CellPickMethod.CircleRect => {
					val cornerA = new Point2D.Float
					val cornerB = new Point2D.Float
					affine.transform(dragOrigin, cornerA)
					affine.transform(dragPoint, cornerB)
					val origin = new Point2D.Float((cornerA.x + cornerB.x) / 2, (cornerA.y + cornerB.y) / 2)
					val radius: Float = abs(cornerA.x - cornerB.x) / 2
					if (cornerA.distance(cornerB) > 1) {
						dataSet.addCell(origin.x, origin.y, radius)
					}
				}
			}
			dragPoint = null
			dragOrigin = null
			parentFrame.repaint
		}
	}

	def judgeAndGetATOfPoint(p: Point): AffineTransform = {
		val w = draw.width
		val h = draw.height
		if (p.x < w && p.y < h) invtrans.bf
		else if (p.x > w && p.y < h) invtrans.ricm
		else if (p.x < w && p.y > h) invtrans.tcr
		else if (p.x > w && p.y > h) invtrans.icam
		else null
	}

	def init: Unit = {
		addMouseListener(this)
		addKeyListener(new KeyAdapter() {
			override def keyPressed(e: KeyEvent) {
				e.getKeyCode match {
					case KeyEvent.VK_RIGHT =>
						parentFrame.moveFileIndex(1)
					case KeyEvent.VK_LEFT =>
						parentFrame.moveFileIndex(-1)
					case KeyEvent.VK_UP =>
						parentFrame.selectCellMove(-1)
					case KeyEvent.VK_DOWN =>
						parentFrame.selectCellMove(1)
					case KeyEvent.VK_BACK_SPACE =>
						val i = parentFrame.selectedCellIndex
						if (i != -1) {
							val c = dataSet.cells(i)
							dataSet.removeCell(Array(c))
						}
						println("back space key")
					case _ =>
				}
			}
		})
	}

	def tr(p: Point, t: AffineTransform): Point2f = tr(new Point2D.Float(p.x, p.y), t)

	def tr(p: Point2D.Float, t: AffineTransform): Point2f = {
		var ret = new Point2D.Float
		t.transform(p, ret).asInstanceOf[Point2D.Float]
		new Point2f(ret.x, ret.y)
	}

	def findCellsFromPosition(p: Point2D.Float): Array[Cell] = {
		var flag = false

		dataSet.cells.filter({
			a: Cell =>
				a.boundary.isInside(tr(p, invtrans.bf)) ||
					a.boundary.isInside(tr(p, invtrans.ricm)) ||
					a.boundary.isInside(tr(p, invtrans.tcr)) ||
					a.boundary.isInside(tr(p, invtrans.icam))
		}).toArray
	}

	def getIndexOfCell(target: Cell): Int = {
		val cells = dataSet.cells;
		for (i <- cells.indices) {
			if (cells(i) == target) {
				return i;
			}
		}
		return -1;
	}

	override def paintComponent(gg: Graphics): Unit = {
		super.paintComponent(gg)
		try {
			//      println("ImgFrame.paintComponent()")
			if (!active)
				return;
			val g = gg.asInstanceOf[Graphics2D]
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
			calcDimension
			notrans = g.getTransform
			im.zip(trans).foreach(a => g.drawImage(a._1, a._2, this))
			val selected = if (parentFrame.selectedCellIndex == -1)
				null
			else
				dataSet.cells(parentFrame.selectedCellIndex)
			for (c <- dataSet.cells) {
				g.setStroke(basicStroke)
				if (c == selected)
					g.setColor(new Color(255, 0, 0, 200))
				else
					g.setColor(new Color(255, 255, 255, 180))
				for (tr <- trans) {
					g.transform(tr)
					g.drawOval(round(c.boundary.cx - c.boundary.r), round(c.boundary.cy - c.boundary.r),
						round(c.boundary.r * 2), round(c.boundary.r * 2))
					g.setTransform(notrans)
				}
				for (tr <- trans) {
					g.transform(tr)
					val dr = (3.0 / scale).toFloat
					g.setColor(new Color(0, 255, 0, 200))
					g.fillOval(round(c.boundary.cx - dr), round(c.boundary.cy - dr),
						round(dr * 2), round(dr * 2))
					g.setTransform(notrans)
				}
				for (tr <- trans) {
					g.transform(tr)
					val dr = (3.0 / scale).toFloat
					g.setColor(new Color(255, 0, 0, 200))
					g.fillOval(round(c.tcrCenter.x - dr), round(c.tcrCenter.y - dr),
						round(dr * 2), round(dr * 2))
					g.setTransform(notrans)
				}
			}
			if (dragOrigin != null) {
				g.setStroke(new BasicStroke(1))
				g.setColor(new Color(255, 0, 255, 255))
				Config.cellPickMethod match {
					case CellPickMethod.CircleOrigin =>
						val r = dragOrigin.distance(dragPoint)
						g.drawOval(round(dragOrigin.x - r).toInt, round(dragOrigin.y - r).toInt, round(r * 2).toInt, round(r * 2).toInt)
					case CellPickMethod.CircleRect =>
						val side = abs(dragOrigin.x - dragPoint.x)
						g.drawRect(dragOrigin.x, dragOrigin.y, side, side)
						g.drawOval(dragOrigin.x, dragOrigin.y, side, side)
				}
			}
		} catch {
			case e: Exception =>
				e.printStackTrace
		}
	}
}