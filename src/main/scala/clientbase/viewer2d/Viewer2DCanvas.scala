package clientbase.viewer2d

import clientbase.control.{FocusOwner, SelectionController}
import definition.data.{EMPTY_REFERENCE, Reference}
import org.denigma.threejs._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{ClientRect, HTMLElement}
import util.{Log, StrToInt}
import scalatags.JsDom.all._

import scala.collection.mutable
import scala.scalajs.js
import scala.util.matching.Regex

/**
  * Created by Peter Holzer on 11.02.2017.
  */

object MouseButtons extends Enumeration {
  val NONE: MouseButtons.Value = Value(0)
  val LEFT: MouseButtons.Value = Value(1)
  val RIGHT: MouseButtons.Value = Value(2)
  val MIDDLE: MouseButtons.Value = Value(4)

  def getButton(i: Int): MouseButtons.Value =
    if ((i & 1) > 0) LEFT else if ((i & 2) > 0) RIGHT else if ((i & 4) > 0) MIDDLE else NONE
}

class Viewer2DCanvas(controller: Viewer2DController, canvHolder: Div, scaleModel: ScaleModel) extends FocusOwner {
  final val tresh = 2
  val IntersectPattern: Regex = "([CDEFLST])(\\d+)".r
  var isPainting=false
  var downButtons:Int=0
  var downX:Double=0
  var downY:Double=0
  val oldTouches: mutable.LinkedHashMap[Double, Touch] = collection.mutable.LinkedHashMap[Double, Touch]()
  var touchX1: Double = 0
  var touchX2: Double = 0
  var touchY1: Double = 0
  var touchY2: Double = 0
  var lastMouseX:Double=0
  var lastMouseY:Double=0
  var currentBounds: ClientRect = _
  val scene = new Scene()
  val whiteColor = new Color(0.98d, 0.98d, .98d)
  var camera = new PerspectiveCamera(90, 1, 0.1, 1000)
  val renderer = new WebGLRenderer(js.Dynamic.literal(alpha = true, clearColor = whiteColor, autoSize = false, antialias = true
  ).asInstanceOf[WebGLRendererParameters])
  val contextListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent) => {e.preventDefault()}
  val rayCaster = new Raycaster()
  val pickTreshold=2
  val pickVector = new Vector2()
  val selectDragTreshold=3
  var isRectDragging:Boolean=false
  val hudCanvas: Canvas =canvas(`class`:="hud-canvas").render
  val pointerCorrX: Double = 0.5d
  val pointerCorrY: Double = -0.5d
  renderer.domElement.style.position="absolute"
  renderer.domElement.style.overflow = "hidden"
  canvHolder.appendChild(hudCanvas)
  canvHolder.appendChild(renderer.domElement)

  val wheelListener:scala.scalajs.js.Function1[WheelEvent, Unit]=(e: WheelEvent) => {
    //println("wheel "+e.deltaMode+" y:"+e.deltaY)
    if (e.deltaY < 0) controller.scaleModel.zoomPlus()
    else if (e.deltaY > 0) controller.scaleModel.zoomMinus()
    e.preventDefault()
    e.stopPropagation()
  }
  val mouseDownListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    println("click "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" "+e.ctrlKey)
    notifyFocus()
    canvHolder.focus()
    downButtons=e.buttons
    isRectDragging=false
    downX=e.clientX
    downY=e.clientY
    lastMouseX=e.clientX
    lastMouseY=e.clientY
    e.preventDefault()
    e.stopPropagation()
  }

  val mouseUpListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    //println("up "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" ctrl:"+e.ctrlKey+" isRectDragging:"+isRectDragging+" cbt:"+currentBounds.top)
    e.preventDefault()
    if(isRectDragging) {
      isRectDragging=false
      currentBounds = canvHolder.getBoundingClientRect()
      controller.rectDragCompleted((downX-currentBounds.left-9.5).toInt,(downY-currentBounds.top).toInt,(e.clientX-currentBounds.left).toInt,(e.clientY-currentBounds.top).toInt,e.ctrlKey,e.shiftKey)
      drawCrossHair(e.clientX,e.clientY)
    } else
      controller.mouseClicked(MouseButtons.getButton(downButtons), downX, downY, e.ctrlKey)
    downButtons=0
  }


  val moveListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=>{   
    //drawCrossHair(lastMouseX,lastMouseY)
    if (downButtons == MouseButtons.MIDDLE.id) {
      val dx=lastMouseX-e.clientX
      val dy=lastMouseY-e.clientY
      /*if (Math.abs(dx) > 2 || Math.abs(dy) > 2) {*/
        if (e.ctrlKey) {
          camera.rotation.y = camera.rotation.y + Math.PI * dx / canvHolder.clientWidth
          camera.rotation.x = camera.rotation.x + Math.PI * dy / canvHolder.clientHeight
          repaint()
        } else {
          controller.scaleModel.move(dx, dy)
        }
      //}
      e.stopPropagation()
      e.preventDefault()
    } else if(downButtons== MouseButtons.LEFT.id) {
      if(controller.controllerState==ControllerState.SelectElems) {
        if(!isRectDragging && !inDistance(e.clientX,e.clientY,downX,downY,selectDragTreshold)) {
          isRectDragging=true
          //selectRectangleVisible(true)
          //println("Start Drag Rect downx:"+downX+" downy:"+downY)
        }
      }
    }
    lastMouseX = e.clientX
    lastMouseY = e.clientY
    drawCrossHair(e.clientX,e.clientY)
  }
  
  def drawCrossHair(mX:Double,mY:Double):Unit = {
    val context  =hudCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    context.globalCompositeOperation = "xor"
    currentBounds = canvHolder.getBoundingClientRect()
    context.clearRect(0,0,hudCanvas.width,hudCanvas.height)
    context.fillStyle="rgb(0,0,0)"
    context.lineWidth=1
    context.beginPath()
    //println("move x:"+mX+" y:"+mY+" target:"+e.currentTarget)
    context.moveTo(0,mY-currentBounds.top-pointerCorrY)
    context.lineTo(hudCanvas.width,mY-currentBounds.top-pointerCorrY)
    context.moveTo(mX-currentBounds.left-pointerCorrX,0.5)
    context.lineTo(mX-currentBounds.left-pointerCorrX,hudCanvas.height-0.5)
    context.stroke()
    if(isRectDragging) {
      context.strokeRect(scala.math.min(mX,downX)-currentBounds.left-pointerCorrX,scala.math.min(mY,downY)-currentBounds.top-pointerCorrY,scala.math.abs(mX-downX),scala.math.abs(mY-downY))
    }
  }

  def cleanUpCrosshairCanvas(e:MouseEvent):Unit ={
    val context  =hudCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    context.clearRect(0,0,hudCanvas.width,hudCanvas.height)
  }

  def notifyFocus():Unit = {
    SelectionController.setFocusedElement(this)
    if (SelectionController.containerFocused(controller,0)) controller.toolbar.showCreateButtons()
    canvHolder.style.border="solid blue 1px"
  }


  renderer.setClearColor(whiteColor, 0.4d)
  camera.position.z = 30
  dom.window.addEventListener("resize", (_: Event) => onResize())
  scaleModel.registerScaleListener(() => adjustCamera())
  canvHolder.addEventListener("contextmenu", contextListener, useCapture = false)
  canvHolder.addEventListener("mouseenter", (e: MouseEvent) => {
  })
  canvHolder.addEventListener("mouseleave", (e: MouseEvent) => {
    if (e.target == canvHolder && downButtons > 0) mouseUpListener.apply(e)
    cleanUpCrosshairCanvas(e)
  })
  canvHolder.addEventListener("wheel", wheelListener, useCapture = true)
  canvHolder.addEventListener("mousedown", mouseDownListener, useCapture = true)
  canvHolder.addEventListener("mouseup", mouseUpListener, useCapture = true)
  canvHolder.addEventListener("mousemove", moveListener, useCapture = true)

  canvHolder.addEventListener("touchstart", (e: TouchEvent) => {
    SelectionController.printMessage("touchstart " + e.targetTouches.length + " " + e.targetTouches.item(0).identifier)
    e.targetTouches.length match {
      case 1 =>
        oldTouches.clear
        val t1 = e.targetTouches.item(0)
        oldTouches(t1.identifier) = t1
        touchX1 = e.targetTouches.item(0).clientX
        touchY1 = e.targetTouches.item(0).clientY
      case 2 =>
        //SelectionController.printMessage("touchstart ")
        val t1 = e.targetTouches.item(0)
        val t2 = e.targetTouches.item(1)
        oldTouches(t1.identifier) = t1
        oldTouches(t2.identifier) = t2
        touchX1 = t1.clientX
        touchY1 = t1.clientY
        touchX2 = t2.clientX
        touchY2 = t2.clientY
        //e.preventDefault()
      case _ =>
    }
    e.stopPropagation()
    e.preventDefault()
  })

  canvHolder.addEventListener("touchmove", (e: TouchEvent) => {
    e.targetTouches.length match {
      case 1 =>
        val t1 = e.targetTouches.item(0)
        oldTouches.get(t1.identifier) match {
          case Some(oldTouch) =>
            val d1x = t1.clientX - oldTouch.clientX
            val d1y = t1.clientY - oldTouch.clientY
            if (Math.abs(d1x) > tresh || Math.abs(d1y) > tresh) {
              controller.scaleModel.move(-d1x, -d1y)
              if (oldTouches.size > 1) {
                println("oldtouches size " + oldTouches.size)
                oldTouches.clear()
              }
              oldTouches(t1.identifier) = t1
            } else controller.mouseClicked(MouseButtons.LEFT, t1.clientX, t1.clientY, controlKey = false)
          case None => SelectionController.printMessage("touchmove touch with identifier not found " + t1.identifier + " oldTouches:" + oldTouches.mkString("|"))
        }

      case 2 =>
        val t1 = e.targetTouches.item(0)
        val t2 = e.targetTouches.item(1)
        val nX1 = t1.clientX
        val nY1 = t1.clientY
        val nX2 = t2.clientX
        val nY2 = t2.clientY
        val d1x = nX1 - touchX1
        val d1y = nY1 - touchY1
        val d2x = nX2 - touchX2
        val d2y = nY2 - touchY2
        val dir1x = getDir(d1x)
        val dir1y = getDir(d1y)
        val dir2x = getDir(d2x)
        val dir2y = getDir(d2y)

        def updateTouch(): Unit = {
          touchX1 = t1.clientX
          touchY1 = t1.clientY
          touchX2 = t2.clientX
          touchY2 = t2.clientY
          oldTouches(t1.identifier) = t1
          oldTouches(t2.identifier) = t2
        }

        if ((Math.abs(d1x) > tresh) || (Math.abs(d1y) > tresh) || (Math.abs(d2x) > tresh) || (Math.abs(d2y) > tresh)) {
          if (dir1x == dir2x && dir1y == dir2y) {
            // move
            val averageX = (Math.abs(d1x) + Math.abs(d2x) + 1) / 2 * Math.signum(d1x)
            val averageY = (Math.abs(d1y) + Math.abs(d2y) + 1) / 2 * Math.signum(d1y)
            controller.scaleModel.move(-averageX, -averageY)
            updateTouch()
          }
          else {
            // zoom
            val commonX = Math.abs(d1x) + Math.abs(d2x)
            val commonY = Math.abs(d1y) + Math.abs(d2y)
            //SelectionController.printMessage("d1x:" + dir1x + " d1y:" + dir1y + " d2x:" + dir2x + " d2y:" + dir2y+" cx:"+commonX+" dy:"+commonY)
            if (commonX > commonY && commonX > tresh + 2) {
              if (((nX1 < nX2) && dir1x == 1) || ((nX2 < nX1) && dir2x == 1) ||
                ((nX1 < nX2) && dir2x == -1) || ((nX2 < nX1) && dir1x == -1)) {
                // zoomout
                controller.scaleModel.zoomOutBy(commonX / canvHolder.clientWidth)
                updateTouch()
              }
              else if (((nX1 < nX2) && dir1x == -1) || ((nX2 < nX1) && dir2x == -1) ||
                ((nX1 < nX2) && dir2x == 1) || ((nX2 < nX1) && dir1x == 1)) {
                controller.scaleModel.zoomInBy(commonX / canvHolder.clientWidth)
                updateTouch()
              }
            } else if (commonY > tresh + 2) {
              if (((nY1 < nY2) && dir1y == 1) || ((nY2 < nY1) && dir2y == 1) ||
                ((nY1 < nY2) && dir2y == -1) || ((nY2 < nY1) && dir1y == -1)) {
                // zoomout
                controller.scaleModel.zoomOutBy(commonY / canvHolder.clientHeight)
                updateTouch()
              }
              else if (((nY1 < nY2) && dir1y == -1) || ((nY2 < nY1) && dir2y == -1) ||
                ((nY1 < nY2) && dir2y == 1) || ((nY2 < nY1) && dir1y == 1)) {
                controller.scaleModel.zoomInBy(commonY / canvHolder.clientHeight)
                updateTouch()
              }
            }
          }

        }
      case _ =>
    }
    //e.preventDefault()
  })

  val touchEndListener: scala.scalajs.js.Function1[TouchEvent, Unit] = (_: TouchEvent) => {
  }

  canvHolder.addEventListener("touchend", touchEndListener)
  canvHolder.addEventListener("touchcangel", touchEndListener)


  def getDir(d: Double): Int = if (Math.abs(d) <= tresh) 0 else if (d < 0d) -1 else 1

  def addGeometry(obj: Object3D): Unit = {
    if (obj != null)
      scene.add(obj)
  }

  def removeGeometry(obj: Object3D): Unit = {
    if (obj != null)
      scene.remove(obj)
  }

  def onResize():Unit= if(canvHolder.clientWidth>0){
    currentBounds = canvHolder.getBoundingClientRect()
    val w = canvHolder.clientWidth - 1
    val h = canvHolder.clientHeight - 1
    //println("resize "+w+" "+h)
    hudCanvas.width=w
    hudCanvas.height=h

    controller.scaleModel.setViewSize(w,h)
    renderer.setSize(w * 1.5d, h * 1.5d, updateStyle = false)
    renderer.domElement.style.width = w + "px"
    renderer.domElement.style.height = h + "px"
    hudCanvas.style.width=renderer.domElement.style.width
    hudCanvas.style.height=renderer.domElement.style.height
    val oldx = camera.position.x
    val oldy = camera.position.y
    val oldz = camera.position.z
    val oldrx = camera.rotation.x
    val oldry = camera.rotation.y
    camera = new PerspectiveCamera(90, w.toDouble / h.toDouble, 0.1, 1000)
    camera.position.x = oldx
    camera.position.y = oldy
    camera.position.z = oldz
    camera.rotation.x = oldrx
    camera.rotation.y = oldry
    camera.updateProjectionMatrix()

    repaint()
  }

  def adjustCamera(): Unit = {
    val (x, y, z) = scaleModel.get2DCameraPos
    camera.position.x = x
    camera.position.y = y
    camera.position.z = z
    repaint()
  }

  def repaint():Unit = if(!isPainting) {
    isPainting=true
    try {
      //val before=System.currentTimeMillis()
      renderer.render(scene, camera)
      //println("Render "+(before-controller.movetime)+" "+(System.currentTimeMillis()-before))
    } catch {
      case e: Throwable => Log.e("render", e)
    }
    isPainting=false
  }

  def pickElems(screenX: Double, screenY: Double): js.Array[Reference] = {
    val viewBonds = renderer.domElement.getBoundingClientRect()
    var testx=0d
    var testy=0d
    val resultSet= collection.mutable.Set[Reference]()

    def addElement(el:Intersection):Unit= {
      el.`object`.name match {
        case IntersectPattern(typString, StrToInt(inst)) ⇒

          val typ = typString match {
            case "C" ⇒ GraphElem.ARCTYPE
            case "D" => GraphElem.DIMLINETYP
            case "E" ⇒ GraphElem.ELLIPSETYP
            case "F" => GraphElem.FILLTYPE
            case "L" ⇒ GraphElem.LINETYPE
            case "S" => GraphElem.SYMBOLTYP
            case "T" => GraphElem.TEXTTYP
            case o ⇒ Log.e("Unknown ElemTyp:" + o); 0
          }
          resultSet+= Reference(typ, inst)
        case o ⇒ Log.e("wrong object name " + o); EMPTY_REFERENCE
      }
    }

    for(i<-0 to 4) {
      i match {
        case 0 => testx=screenX;testy=screenY
        case 1 => testx=screenX+pickTreshold;testy=screenY
        case 2 => testx=screenX-pickTreshold;testy=screenY
        case 3 => testx=screenX;testy=screenY+pickTreshold
        case 4 => testx=screenX;testy=screenY-pickTreshold
      }
      pickVector.x = ((testx - viewBonds.left) / viewBonds.width) * 2d - 1d
      pickVector.y = 1d - ((testy - viewBonds.top) / viewBonds.height) * 2d

      rayCaster.setFromCamera(pickVector, camera)
      val intersects: js.Array[Intersection] = rayCaster.intersectObjects(scene.children)
      //println("r "+i+" intersects:"+intersects.length)
      for (el: Intersection ← intersects) addElement(el)
    }
    val result=new js.Array[Reference]
    for(el<-resultSet) result.push(el)
    result
  }

  def inDistance(x1:Double,y1:Double,x2:Double,y2:Double,distance:Double): Boolean =
    scala.math.abs(x1-x2)<distance && scala.math.abs(y1-y2)<distance


  def blur():Unit ={
    canvHolder.blur()
    canvHolder.style.removeProperty("border")
    repaint()
  }



}
