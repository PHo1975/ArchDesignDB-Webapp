package clientbase.tableview

import clientbase.viewer2d.BlockTestModule
import definition.data.Reference
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


trait PluginModule {
  def moduleName:String
  def fullSize:Boolean
  def content:HTMLElement
  def shutDown():Unit
  def load(ref:Reference):Unit
}

object PluginModules {
  protected var templateMap: Map[Int, Class[_ <: PluginModule]] =Map[Int, Class[_ <:PluginModule]](
    700->classOf[BlockTestModule])

  protected val loadedPluginsMap: collection.mutable.Map[Int, PluginModule] =collection.mutable.HashMap[Int,PluginModule]()

  def contains(typ:Int): Boolean =templateMap.contains(typ)

  def apply(typ:Int): PluginModule = loadedPluginsMap.getOrElseUpdate(typ,instantiate(templateMap(typ))())

  def instantiate[A](cls: Class[A])(args: Any*): A = {
    val gb=js.Dynamic.global
    val ctor = gb.selectDynamic(cls.getName.split("\\.").last)
    js.Dynamic.newInstance(ctor)(args.asInstanceOf[Seq[js.Any]]: _*).asInstanceOf[A]
  }
}


