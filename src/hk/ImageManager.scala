package hk

import java.io.File
import ij.ImagePlus
import ij.io.Opener
import ij.ImageStack
import ij.process.{AutoThresholder, ImageProcessor}


object ImageManager{
	def createImageManager(m: Path, mode: Int): Option[ImageManager] = {
		val folder: Path = m.parent.parent
		mode match {
			case 1 =>
				FolderImageManager.fromFolder(folder)
			case 2 =>
				val subFolder: String = m.parent.name
				val stkFile = folder.child(subFolder.drop(9) + ".stk")
				StackImageManager.fromStackFile(stkFile)
		}
	}
	def imageExists(m: Path, mode: Int): Boolean = {
		val im = createImageManager(m,mode)
		im match {
			case Some(i) =>
				i.release()
				true
			case None =>
				false
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
  def release() {imps.foreach(imp=>{
	  imp.close()
  })}
	def loadImages()
}

class StackImageManager extends ImageManager {
	var stkFile: Path = _
	def identityPath = stkFile.path
	def this(stk: Path) = {
		this
		stkFile = stk
		loadImages()
	}
	def loadImages() {
		val imp = new ImagePlus(stkFile.path)
		imps = Array(imp)
		val stack: ImageStack = imp.getStack
		//		println("Loading: " + stkFile)
		if(stack.getSize!=4)
			throw new StackFileIncorrectException(".stk format is incorrect: "+stkFile.path)
		_ip = new Con4[ImageProcessor](Array(1,2,4,3).map(i=>{
			val ip = stack.getProcessor(i)
			ip.resetMinAndMax()
			ip
		}))
	}
}

object StackImageManager {
	def fromStackFile(stk: Path): Option[StackImageManager] = {
		try{
		  Some(new StackImageManager(stk))
		}catch{
			case _ =>
				None
		}
	}

}

class StackFileIncorrectException(t: String) extends Exception(t)

class FolderImageManager extends ImageManager {
	def identityPath = folder.path
	var folder: Path = _
	private def this(_folder: Path) = {
	  this
	  folder = _folder
		loadImages()
	}
	def loadImages() {
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

object FolderImageManager {
	def fromFolder(folder: Path): Option[FolderImageManager] = {
	try{
			Some(new FolderImageManager(folder))
		}catch{
			case _ => None
		}
	}
}