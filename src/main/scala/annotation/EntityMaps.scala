package annotation

import java.io.File
import java.util.concurrent.ConcurrentHashMap

/**
  * Created by xiaziyao on 2017/4/20.
  */
object EntityMaps {
  private val entities = new ConcurrentHashMap[String, Table]()
  def getEntity(name: String) = Option(entities.get(name))
  def put(name: String, entity: Table) = entities.put(name, entity)
  def getEntities = entities

  def init(packageName: String) = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val path = packageName.replace('.', '/')
    val DOT_CLASS = 6 //".class".length = 6
    val resources = classLoader.getResources(path)
    while (resources.hasMoreElements) {
      val file = new File(resources.nextElement().getFile)
      if(file.isDirectory) {
        file.listFiles().foreach { f =>
          //println("====init class : "packageName + '.' + f.getName.substring(0, f.getName.length() - 6))
          if(f.getName.contains("$"))
            Class.forName(packageName + '.' + f.getName.substring(0, f.getName.length() - DOT_CLASS))
        }
      }
    }
    //println("====init end")
  }
}

case class Table(name: String, fields: List[Field])
