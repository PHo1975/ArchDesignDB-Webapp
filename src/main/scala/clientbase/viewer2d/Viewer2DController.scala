package clientbase.viewer2d

import clientbase.control.SidepanelController
import clientbase.tilelayout.TileContent
import definition.data.Referencable
import org.scalajs.dom.Element
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import util.Log

import scala.util.control.NonFatal
import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 11.02.2017.
  */
class Viewer2DController extends TileContent with ElemContainer {
  val zoomAllBut=button(onclick:={(e:MouseEvent)=>{zoomAll()}})("Alles").render

  val layerList=new LayerList(this)
  val scaleModel=new ScaleModel

  val myCanvas: Canvas =canvas(`class`:="viewer2dcanvas").render
  val horCross: Div = div(`class`:="crosshairdivs").render
  val vertCross: Div = div(`class`:="crosshairdivs").render

  override val content: HTMLElement = div(`class`:="viewer2dcontent")(
    div(`class`:="viewer2dbuttonbar")(zoomAllBut),myCanvas,horCross,vertCross).render

  val canvasHandler=new Viewer2DCanvas(this,myCanvas,horCross,vertCross)
  scaleModel.registerScaleListener(()=>canvasHandler.onResize())

  def scaleRatio:Double= scaleModel.relativeScaleValue

  override def init(selection: Iterable[Referencable]): Unit = {
   //println("Viewer init "+selection.mkString("|"))
    try {
      layerList.loadLayers(selection)
    } catch {
      case NonFatal(e) => Log.e("Init",e)
    }
  }

  override def load(): Unit = {}
  override def save(): Unit = {}


  override def close(): Unit = {
    layerList.shutDown()
  }

  override def getSelection: Iterable[Referencable] = Nil

  def readyLoaded():Unit={
    zoomAll()
    dataUpdated()
  }

  def dataUpdated():Unit= if(layerList.loaded){
    canvasHandler.onResize()
  }

  override def updateResize():Unit={
    canvasHandler.onResize()
  }

  def zoomAll():Unit= {
    val bounds=layerList.calcBounds()
    if(bounds.maxX==Double.MaxValue){ // no elements in layer, still max value
      util.Log.w("ZoomAll bounds=null "+bounds)
      scaleModel.setWorldBounds(-1,-1,5,5)
    }
    else scaleModel.setWorldBounds(bounds.minX,bounds.minY,bounds.maxX-bounds.minX,bounds.maxY-bounds.minY)
  }

  def setVisibleScale(scaleID:Int ):Unit = scaleModel.setRelativeScaleID(scaleID)


}
