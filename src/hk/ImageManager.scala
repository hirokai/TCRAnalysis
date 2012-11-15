package hk

import java.io.File
import ij.ImagePlus
import ij.io.Opener
import ij.ImageStack
import ij.process.ImageProcessor


object ImageManager{
  def createImageManager(m: Path, mode: Int) = {
    val folder: Path = m.parent.parent
	mode match {
	case 1 =>
		new FolderImageManager(folder)
	case 2 =>
	  	val subFolder: String = m.parent.name
		val stkFile = folder.child(subFolder.drop(9) + ".stk")
		new StackImageManager(stkFile)
	}
  }
}

abstract class ImageManager{
  def identityPath: String
  var _ip: Con4[ImageProcessor] = _
  def ip = _ip
  def width = ip.bf.getWidth
  def height = ip.bf.getHeight
  var imps: Array[ImagePlus] = _
  def release() {imps.foreach(imp=>imp.flush())}
}

class StackImageManager extends ImageManager {
	var stkFile: Path = _
	def identityPath = stkFile.path
	def this(stk: Path) = {
		this
		stkFile = stk
  	  	val imp = new ImagePlus(stkFile.path)
		imps = Array(imp)
		val stack: ImageStack = imp.getStack
		println("Loading: " + stkFile)
	  	if(stack.getSize!=4)
	  		throw new StackFileIncorrectException(".stk format is incorrect: "+stkFile.path)
	  	_ip = new Con4[ImageProcessor](Array(1,2,4,3).map(stack.getProcessor(_)))
	}
}

class StackFileIncorrectException(t: String) extends Exception(t)

class FolderImageManager extends ImageManager {
	def identityPath = folder.path
	var folder: Path = _
	def this(_folder: Path) = {
	  this
	  folder = _folder
	  if(!folder.isDir)
	    throw new Exception("FolderImageManager: not a folder.")
	  println("Loading folder: "+folder)
	  val f = Config.filename; val sep = File.separator
	  imps = f.map({fn: String =>
	        (new Opener).openImage(folder.child(fn).path)
	        }).toArray
	  _ip = new Con4[ImageProcessor](imps.map(_.getProcessor).toArray)
	}
}
