package clientbase.viewer2d

import clientbase.control.{FocusContainer, PointClickListener, SelectionController}
import clientbase.tilelayout.TileContent
import definition.data.{Referencable, Reference}
import definition.expression.{NULLVECTOR, VectorConstant}
import definition.typ.SelectGroup
import org.denigma.threejs.{Camera, Object3D, Vector3}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{ClientRect, HTMLElement}
import scalatags.JsDom.all._
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.util.control.NonFatal

object ControllerState extends Enumeration {
  val SelectElems: ControllerState.Value = Value("Select")
  val AskPoint: ControllerState.Value = Value("AskPoint")
  val AskObject: ControllerState.Value = Value("Chose")
  val AskPointOrObject: ControllerState.Value = Value("PointOrObject")
  val InPlaceEdit: ControllerState.Value = Value("InPlaceEdit")
  val SelectPoints: ControllerState.Value = Value("SelectPoints")
  val DragDrop: ControllerState.Value = Value("DragDrop")
}

object Viewer2DController{
  val tempElementColor: Int =255<<16+30<<8
}

trait AbstractViewerController extends FocusContainer {
  def focus():Unit
  def resetPointSelection():Unit
  def askForPoint(el:PointClickListener):Unit
  def bracketPointer:VectorConstant
  def startBracketMode():Unit
  def bracketMode:Boolean
  def clearTempElements():Unit
  def addTempElement(newElem:GraphElem):Unit
}


case class HitInfo(worldPos:VectorConstant, screenX:Double, screenY:Double)
case class PointMatchInfo(hitBoth:Option[HitInfo],hitX:Option[HitInfo],hitY:Option[HitInfo])

class Viewer2DController extends AbstractViewerController with TileContent with ElemContainer {
  val layerPan = new LayerListPan(this)
  var bracketPointer:VectorConstant=NULLVECTOR
  var bracketMode:Boolean=false
  val toolbar=new Toolbar(this)
  val layerList=new LayerList(this)
  val scaleModel=new ScaleModel
  val canvasHolder: Div = div(`class` := "viewer2dcanvas",tabindex:="0")(/*horCross, vertCross,selectRectangle*/).render
  val geometryBuffer: ArrayBuffer[Object3D] =collection.mutable.ArrayBuffer[Object3D]()
  var customDragger:Option[(VectorConstant,CanvasRenderingContext2D)=>Unit]=None
  var lastSelectedPoint:VectorConstant=NULLVECTOR
  var pointClickListener :Option[PointClickListener]=None
  var _hasCreateActionStarted=false
  var numCreatedElements=0
  final val pointCatchDistance=8d

  override val content: HTMLElement = div(`class`:="viewer2dcontent")(layerPan.pane,toolbar.toolbarDiv , canvasHolder).render

  val canvasHandler = new Viewer2DCanvas(this, canvasHolder, scaleModel)
  var controllerState: ControllerState.Value = ControllerState.SelectElems

  def scaleRatio:Double=  scaleModel.relativeScaleValue
  def scaleToString(rel: Double): String = if (rel < 1) "1 : " + math.round(1d / rel) else math.round(rel).toString + " : 1"

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

  def dataUpdated():Unit= if(layerList.loaded) canvasHandler.repaint()

  override def updateResize():Unit={
    canvasHandler.onResize()
  }

  def zoomAll():Unit= {
    val bounds=layerList.calcBounds()
    //Log.e("zoomall size:"+layerList.subscriberList.size+" mx:"+bounds.minX+" my:"+bounds.minY+" max:"+bounds.maxX+
    //"maxy:"+bounds.maxY)
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
      case MouseButtons.LEFT =>
        println("clicked "+controllerState+" pos:"+screenx+","+screeny)
        controllerState match {
          case ControllerState.SelectElems =>
            val elems: js.Array[Reference] = canvasHandler.pickElems(screenx, screeny)
            val newSelection: Seq[SelectGroup[_ <: Referencable]] = layerList.decodeSelection(elems)
            if (controlKey) {
              SelectionController.addSelection(newSelection, toggle = true)
            }
            else SelectionController.select(newSelection)
            dataUpdated()
          case ControllerState.AskPoint =>
            for (li <- pointClickListener) {
              val hitPos = checkHitPoints(screenx, screeny, canvasHandler.camera)
              //resetState()
              if (li.forcePrecision)
                for (p <- hitPos.hitBoth)
                  li.pointClicked(p.worldPos)
            }
          case _ =>
        }
      case _ =>
    }

  def rectDragCompleted (startPointx:Int,startPointy:Int,endPointx:Int,endPointy:Int,controlKey:Boolean,shiftKey:Boolean): Unit ={
    controllerState match {
      case ControllerState.SelectElems=> checkSelection(startPointx,startPointy,endPointx,endPointy,controlKey)

      case _ =>
    }
  }

  def toScreenPosition(x:Double,y:Double):Vector3 = {
    val vector: Vector3 = new Vector3()
    val viewBonds: ClientRect = canvasHandler.renderer.domElement.getBoundingClientRect()
    val widthHalf= 0.5*viewBonds.width
    val heightHalf= 0.5*viewBonds.height
    //vector.setFromMatrixPosition(obj.matrixWorld)
    vector.x=x
    vector.y=y
    vector.z=0
    vector.project(canvasHandler.camera)
    vector.x=vector.x*widthHalf+widthHalf+canvasHandler.currentBounds.left+canvasHandler.pointerCorrX
    vector.y= -vector.y*heightHalf+heightHalf+canvasHandler.currentBounds.top+canvasHandler.pointerCorrY
    vector
  }

  def checkHitPoints(mouseX:Double,mouseY:Double,camera:Camera):PointMatchInfo={
    val vector: Vector3 = new Vector3()
    val viewBonds: ClientRect = canvasHandler.renderer.domElement.getBoundingClientRect()
    val widthHalf= 0.5*viewBonds.width
    val heightHalf= 0.5*viewBonds.height
    val catchDistance: Double =pointCatchDistance/widthHalf
    val px=(mouseX-widthHalf)/widthHalf
    val py= -1.0*(mouseY-heightHalf)/heightHalf

    var bestHitPoint:Option[HitInfo]=None
    var bestHitDistance:Double=Double.MaxValue


    layerList.iterateVisiblePoints(this,point => {
      vector.x=point.x
      vector.y=point.y
      vector.z=0
      vector.project(camera)
      val hitX=Math.abs(vector.x-px)<catchDistance
      val hitY=Math.abs(vector.y-py)<catchDistance
      //if(hitY) println("hit y"+point +" cd:"+catchDistance+" py:"+py+" vy:"+vector.y)
      if(hitX&&hitY) {
        val dist=Math.sqrt((vector.x-px)*(vector.x-px)+(vector.y-py)*(vector.y-py))
        if (dist<bestHitDistance) {
          bestHitDistance=dist
          bestHitPoint=Some(HitInfo(point,vector.x*widthHalf+widthHalf,-vector.y*heightHalf+heightHalf))
        }
        //println("Hit dist:"+dist+" hitinfo:"+bestHitPoint)
      }
    })
    PointMatchInfo(bestHitPoint,None,None)
  }


  def checkSelection(startPointx:Int,startPointy:Int,endPointx:Int,endPointy:Int,controlKey:Boolean): Unit = {
    val viewBonds: ClientRect = canvasHandler.renderer.domElement.getBoundingClientRect()
    val widthHalf= 0.5*viewBonds.width
    val heightHalf= 0.5*viewBonds.height
    val minX=scala.math.min(startPointx,endPointx)
    val maxX=scala.math.max(startPointx,endPointx)
    val minY=scala.math.min(startPointy,endPointy)
    val maxY=scala.math.max(startPointy,endPointy)
    val boundsContainer=new BoundsContainer
    //println("Check Select minx:"+minX+" maxX:"+maxX+" miny:"+minY+" maxY:"+maxY)
    val elList: Iterable[SelectGroup[_ <: Referencable]] =layerList.filterElements(onlyEdible = true, graphElem=>{
      graphElem.calcScreenBounds(this,canvasHandler.camera,boundsContainer)
      if(! boundsContainer.isEmpty) {
        val screenMinX = boundsContainer.minX * widthHalf + widthHalf
        val screenMaxX = boundsContainer.maxX * widthHalf + widthHalf
        val screenMaxY = -boundsContainer.minY * heightHalf + heightHalf
        val screenMinY = -boundsContainer.maxY * heightHalf + heightHalf
        //println("elem "+graphElem.ref+" MinX:"+screenMinX+" MaxX:"+screenMaxX+" minY:"+screenMinY+" maxY:"+screenMaxY)
        screenMinX > minX && screenMaxX < maxX && screenMinY > minY && screenMaxY < maxY
      } else false
      //if(result) println("hit "+graphElem.ref+" MinX:"+screenMinX+" MaxX:"+screenMaxX+" minY:"+screenMinY+" maxY:"+screenMaxY)
    })
    if(elList.nonEmpty)
      if(controlKey) SelectionController.addSelection(elList,toggle = false)
      else SelectionController.select(elList)
    dataUpdated()
  }


  override def addGeometry(gr: Object3D): Unit = try {
    if (gr != null) {
      canvasHandler.addGeometry(gr)
    }
  } catch {
    case e: Throwable => Log.e("add geometry " + gr, e)
  }


  def resetState(): Unit = {
    //println("2d-Controller reset state")
    controllerState match {
      case ControllerState.AskPoint =>
      case _ =>
    }
    controllerState=ControllerState.SelectElems
    bracketMode=false
    canvasHandler.repaint()
    //customDragger=None
    pointClickListener=None
  }

  def focus():Unit = canvasHolder.focus()
  def resetPointSelection():Unit = {
     resetState()
  }

  def askForPoint(listener:PointClickListener):Unit = {
    resetState()
    pointClickListener=Some(listener)
    controllerState=ControllerState.AskPoint
    //println("ask for point")
  }

  override def startBracketMode(): Unit = {
    bracketMode=true
  }

  override def containerName: String = "Graph2DEdit"
  override def getOwnerRef: Option[Referencable] =  layerList.activeLayer
  override def requestFocus(): Unit = canvasHolder.focus()

  override def actionStopped():Unit = {
    resetState()
    clearTempElements()
    customDragger=None
  }
  override def lostSelection():Unit ={
    canvasHandler.repaint()
    clearTempElements()
  }

  def setCustomDragger(ncustomDragger:(VectorConstant,CanvasRenderingContext2D)=>Unit):Unit ={
    customDragger=Some(ncustomDragger)
  }

  override def clearTempElements(): Unit = {
    for(el<-layerList.tempElements;geom<-el.geometry)
      canvasHandler.removeGeometry(geom)
    layerList.tempElements.clear()
  }

  override def addTempElement(newElem: GraphElem): Unit = {
    layerList.tempElements+=newElem
    newElem.createGeometry(this)
    canvasHandler.repaint()
  }

  override def createActionSubmitted(numElements:Int): Unit ={
    _hasCreateActionStarted=true
    numCreatedElements=numElements
  }

  override def resetCreateAction(): Unit ={
    super.resetCreateAction()
    _hasCreateActionStarted=false
    numCreatedElements=0
  }

  def graphElemAdded(lay:LayerSubscriber,elem:GraphElem): Unit ={
    println("GraphElemAdded lay: "+lay+" elem: "+elem)
    if(hasCreateActionStarted) {
      numCreatedElements-= 1
      val selection=SelectGroup(lay.ownerReference,Seq(elem))
      SelectionController.addSelection(List(selection),toggle=false)
      if(numCreatedElements<1)   createdDataReceived()
    }
  }
}
