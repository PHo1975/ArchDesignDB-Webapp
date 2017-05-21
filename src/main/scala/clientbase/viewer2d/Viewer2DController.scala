package clientbase.viewer2d

import clientbase.control.SelectionController
import clientbase.tilelayout.TileContent
import definition.data.Referencable
import org.scalajs.dom.html.{ Button, Div, Select, Option ⇒ DomOption }
import org.scalajs.dom.raw.{ Event, HTMLElement, MouseEvent }
import util.Log
import scala.util.control.NonFatal
import scalatags.JsDom.all._


/**
  * Created by Peter Holzer on 11.02.2017.
  */
class Viewer2DController extends TileContent with ElemContainer {
  val layerPan = new LayerListPan(this)
  val zoomAllBut: Button = button(onclick := { (e: MouseEvent) => {
    zoomAll()
  }
  })("Alles").render
  val layerListBut: Button = button(onclick := { (e: MouseEvent) => {
    layerPan.toggleVisibility()
  }
  })("Layer").render
  val layerList=new LayerList(this)
  val scaleModel=new ScaleModel
  val scaleSelect: Select = select(`class` := "scales-combo").render
  val canvasHolder: Div = div(`class` := "viewer2dcanvas").render
  val horCross: Div = div(`class`:="crosshairdivs").render
  val vertCross: Div = div(`class`:="crosshairdivs").render
  Log.w("create Viewer ")

  override val content: HTMLElement = div(`class`:="viewer2dcontent")(
    layerPan.pane,
    div(`class` := "viewer2dbuttonbar")(layerListBut, zoomAllBut, if (SelectionController.supportsTouch) div() else scaleSelect),
    canvasHolder, horCross, vertCross).render

  val canvasHandler = new Viewer2DCanvas(this, canvasHolder, horCross, vertCross, scaleModel)

  for ((id, sc) <- ScaleModel.scales) scaleSelect.appendChild(option(attr("sid") := id.toString)(scaleToString(sc)).render)
  scaleSelect.onchange = (e: Event) => {
    scaleSelect.selectedIndex match {
      case -1 =>
      case ix => ScaleModel.scales(ix)
    }

  }


  def scaleRatio:Double= scaleModel.relativeScaleValue

  def scaleToString(rel: Double): String = if (rel < 1) "1 : " + math.round(1d / rel) else math.round(rel) + " : 1"

  override def init(selection: Iterable[Referencable]): Unit =
    try {
      layerList.loadLayers(selection)
    } catch {
      case NonFatal(e) => Log.e("Init",e)
    }


  override def load(): Unit = {}
  override def save(): Unit = {}


  override def close(): Unit = {
    layerList.shutDown()
  }

  override def getSelection: Iterable[Referencable] = Nil

  def readyLoaded():Unit={
    for (l <- layerList.subscriberList) layerPan.addLayer(l)
    println("ready loaded " + layerList.subscriberList.length)
    zoomAll()
    dataUpdated()
  }

  def dataUpdated():Unit= if(layerList.loaded){
    canvasHandler.repaint()
  } else println("Updated " + layerList.loaded)

  override def updateResize():Unit={
    canvasHandler.onResize()
  }

  def zoomAll():Unit= {
    val bounds=layerList.calcBounds()
    /*println("zoomall size:"+layerList.subscriberList.size+" mx:"+bounds.minX+" my:"+bounds.minY+" max:"+bounds.maxX+
    "maxy:"+bounds.maxY)*/
    if(bounds.maxX==Double.MaxValue){ // no elements in layer, still max value
      util.Log.w("ZoomAll bounds=null "+bounds)
      scaleModel.setWorldBounds(-1,-1,5,5)
      canvasHandler.adjustCamera()
    }
    else {
      scaleModel.setWorldBounds(bounds.minX, bounds.minY, bounds.maxX - bounds.minX, bounds.maxY - bounds.minY)
      canvasHandler.adjustCamera()
    }
  }

  def setVisibleScale(scaleID:Int ):Unit = scaleModel.setRelativeScaleID(scaleID)

  def setActiveLayerScale(scaleID: Int): Unit = {

  }

}
