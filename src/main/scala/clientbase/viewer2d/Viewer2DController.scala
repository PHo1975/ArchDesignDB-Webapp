package clientbase.viewer2d

import clientbase.control.SelectionController
import clientbase.tilelayout.TileContent
import definition.data.{Referencable, Reference}
import definition.typ.SelectGroup
import org.denigma.threejs.Object3D
import org.scalajs.dom.html.{Button, Div, Select, Option => DomOption}
import org.scalajs.dom.raw.{Event, HTMLElement, MouseEvent}
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scalatags.JsDom.all._

import scala.scalajs.js

object ControllerState extends Enumeration {
  val SelectElems: ControllerState.Value = Value("Select")
  val AskPoint: ControllerState.Value = Value("AskPoint")
  val AskObject: ControllerState.Value = Value("Chose")
  val AskPointOrObject: ControllerState.Value = Value("PointOrObject")
  val InPlaceEdit: ControllerState.Value = Value("InPlaceEdit")
  val SelectPoints: ControllerState.Value = Value("SelectPoints")
  val DragDrop: ControllerState.Value = Value("DragDrop")
}

/**
  * Created by Peter Holzer on 11.02.2017.
  */
class Viewer2DController extends TileContent with ElemContainer {
  val layerPan = new LayerListPan(this)
  val zoomAllBut: Button = button(onclick := { _: MouseEvent => {
    zoomAll()
  }
  })("Alles").render
  val layerListBut: Button = button(onclick := { _: MouseEvent => {
    layerPan.toggleVisibility()
  }
  })("Layer").render
  val layerList=new LayerList(this)
  val scaleModel=new ScaleModel
  val scaleSelect: Select = select(`class` := "scales-combo").render
  val horCross: Div = div(`class`:="crosshairdivs").render
  val vertCross: Div = div(`class`:="crosshairdivs").render
  val canvasHolder: Div = div(`class` := "viewer2dcanvas")(horCross, vertCross).render
  val geometryBuffer: ArrayBuffer[Object3D] =collection.mutable.ArrayBuffer[Object3D]()

  override val content: HTMLElement = div(`class`:="viewer2dcontent")(
    layerPan.pane,
    div(`class` := "viewer2dbuttonbar")(layerListBut, zoomAllBut, if (SelectionController.supportsTouch) div() else scaleSelect),
    canvasHolder).render

  val canvasHandler = new Viewer2DCanvas(this, canvasHolder, horCross, vertCross, scaleModel)
  var controllerState: ControllerState.Value = ControllerState.SelectElems
  //var movetime:Long=0

  for ((id, sc) <- ScaleModel.scales) scaleSelect.appendChild(option(attr("sid") := id.toString)(scaleToString(sc)).render)
  scaleSelect.onchange = (_: Event) => {
    scaleSelect.selectedIndex match {
      case -1 =>
      case ix => ScaleModel.scales(ix)
    }
  }

  def scaleRatio:Double= {
    scaleModel.relativeScaleValue
  }

  def scaleToString(rel: Double): String = if (rel < 1) "1 : " + math.round(1d / rel) else math.round(rel) + " : 1"

  // init from parent Tile
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
    Log.w("ready loaded " + layerList.subscriberList.length)
    zoomAll()
    dataUpdated()
  }

  def dataUpdated():Unit= if(layerList.loaded){
    //Log.w("Update "+layerList.loaded)
    canvasHandler.repaint()
  }

  override def updateResize():Unit={
    canvasHandler.onResize()
  }

  def zoomAll():Unit= {
    val bounds=layerList.calcBounds()
    Log.e("zoomall size:"+layerList.subscriberList.size+" mx:"+bounds.minX+" my:"+bounds.minY+" max:"+bounds.maxX+
    "maxy:"+bounds.maxY)
    if(bounds.maxX==Short.MaxValue.toDouble){ // no elements in layer, still max value
      //util.Log.w("ZoomAll bounds=null "+bounds)
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

  // Interaction

  def mouseClicked(button: MouseButtons.Value, screenx: Double, screeny: Double, controlKey: Boolean): Unit =
    button match {
      case MouseButtons.LEFT ⇒
        controllerState match {
          case ControllerState.SelectElems ⇒
            val elems: js.Array[Reference] = canvasHandler.pickElems(screenx, screeny)
            println("elems:" + elems.mkString(", "))
            val newSelection: Seq[SelectGroup[_ <: Referencable]] = layerList.decodeSelection(elems)
            println("new selection:" + newSelection.mkString(", "))
            SelectionController.select(newSelection)
            dataUpdated()
          case _ ⇒
        }
      case _ ⇒
    }


  override def addGeometry(gr: Object3D): Unit = try {
    if (gr != null) {
      canvasHandler.addGeometry(gr)
    }
  } catch {
    case e: Throwable => Log.e("add geometry " + gr, e)
  }
}
