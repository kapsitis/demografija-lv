package lv.ddgatve.demografija.main

import java.io.File
import lv.ddgatve.scp.NewScpTo
import scala.xml.XML

object DemografijaMain {
  def listDirectoryByPattern(path: String, regex: String): List[File] = {
    val dir = new File(path)
    val myBigFileArray = dir.listFiles()
    val result = myBigFileArray.filter(f => regex.r.findFirstIn(f.getName).isDefined)
    result.toList
  }

  def main(args: Array[String]): Unit = {
    val xml = XML.loadFile("src/main/resources/site-properties.xml")
    val some = for (item <- xml \\ "site" \\ "directories" \ "item") yield {
      val dir = item.text

      val files = listDirectoryByPattern(dir, """.*\.(csv|docx|gif|html|png|pptx|R|rmd)$""")
      println("Directory " + dir + " has " + files.size + " files")
      files map (ff => (dir, ff.getName()))
    }
    val theList = some.toList.flatten
    val lfile = theList map (x => x._1 + "/" + x._2)
    val rfile = theList map (x => "/home/lighttpd/demografija.lv/http/" + x._1 + "/" + x._2)
    println("Copying " + lfile.size + " files to remote server")
    NewScpTo.copyToRemote(lfile, rfile)
  }
}