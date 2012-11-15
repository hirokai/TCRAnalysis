package hk;

import java.io.File

class Path(private val _file: File) {
	def this(path: String) = {
	  this(new File(path))
	}
	def file: File = _file
	def parent = new Path(file.getParentFile)
	def child(name: String) = 
	  new Path(file.getPath + File.separator + name)
	def name = file.getName
	def path = file.getPath
	override def toString = file.getPath
	def isDir = file.isDirectory
	def isFile = file.isFile
	def exists = file.exists
}

object Path{
  def apply(file: File) = {
    new Path(file)
  }
}
