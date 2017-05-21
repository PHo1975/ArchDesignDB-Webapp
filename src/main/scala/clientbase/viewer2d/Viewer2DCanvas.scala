package clientbase.viewer2d

import clientbase.control.SelectionController
import org.denigma.threejs._
import org.scalajs.dom
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._
import util.Log
import scala.collection.mutable
import scala.scalajs.js
/**
  * Created by Peter Holzer on 11.02.2017.
  */


class Viewer2DCanvas(controller: Viewer2DController, canv: Div, horCross: HTMLElement, vertCross: HTMLElement,
                     scaleModel: ScaleModel) {
  val tresh = 2
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
  val scene = new Scene()
  val whiteColor = new Color(0.98d, 0.98d, .98d)
  var camera = new PerspectiveCamera(90, 1, 0.1, 1000)
  val renderer = new WebGLRenderer(js.Dynamic.literal(alpha = true, clearColor = whiteColor, autoSize = true, antialias = true
  ).asInstanceOf[WebGLRendererParameters])
  val contextListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent) => {e.preventDefault()}

  val wheelListener:scala.scalajs.js.Function1[WheelEvent, Unit]=(e: WheelEvent) => {
    //println("wheel "+e.deltaMode+" y:"+e.deltaY)
    if (e.deltaY < 0) controller.scaleModel.zoomPlus()
    else if (e.deltaY > 0) controller.scaleModel.zoomMinus()
    e.preventDefault()
    e.stopPropagation()
  }
  val mouseDownListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    //println("click "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" "+e.ctrlKey)
    downButtons=e.buttons
    downX=e.clientX
    downY=e.clientY
    lastMouseX=e.clientX
    lastMouseY=e.clientY
    e.preventDefault()
    e.stopPropagation()
  }

  val moveListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=>{
    val rect=canv.parentElement.getBoundingClientRect()
    horCross.style.top=(e.clientY-rect.top+4).toString+"px"
    vertCross.style.left=(e.clientX-rect.left+4).toString+"px"
    if(downButtons==4){
      val dx=lastMouseX-e.clientX
      val dy=lastMouseY-e.clientY
      if (e.ctrlKey) {
        camera.rotation.y = camera.rotation.y + Math.PI * dx / canv.clientWidth
        camera.rotation.x = camera.rotation.x + Math.PI * dy / canv.clientHeight
        repaint()
      } else {
        controller.scaleModel.move(dx, dy)
      }
      lastMouseX=e.clientX
      lastMouseY=e.clientY
      e.stopPropagation()
      e.preventDefault()
    }

  }

  val mouseUpListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    //println("up "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" "+e.ctrlKey)
    //e.stopPropagation()
    e.preventDefault()
    downButtons=0
  }

  canv.appendChild(renderer.domElement)
  renderer.setClearColor(whiteColor, 0.4d)
  camera.position.z = 30
  dom.window.addEventListener("resize", (e: Event) => onResize())
  scaleModel.registerScaleListener(() => adjustCamera())
  canv.addEventListener("contextmenu", contextListener, useCapture = false)
  horCross.addEventListener("contextmenu", contextListener, useCapture = false)
  vertCross.addEventListener("contextmenu", contextListener, useCapture = false)
  canv.addEventListener("mouseenter", (e: MouseEvent) => {
    if (!SelectionController.supportsTouch) {
      horCross.style.visibility = "visible"
      vertCross.style.visibility = "visible"
    }
  })
  canv.addEventListener("mouseleave", (e: MouseEvent) => {
    horCross.style.visibility = "hidden"
    vertCross.style.visibility = "hidden"
    if (e.target == canv && downButtons > 0) mouseUpListener.apply(e)
  })
  canv.addEventListener("wheel", wheelListener, useCapture = true)
  canv.addEventListener("mousedown", mouseDownListener, useCapture = true)
  canv.addEventListener("mouseup", mouseUpListener, useCapture = true)
  canv.addEventListener("mousemove", moveListener, useCapture = true)

  canv.addEventListener("touchstart", (e: TouchEvent) => {
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
        e.preventDefault()
      case _ =>
    }
    e.preventDefault()
  })

  canv.addEventListener("touchmove", (e: TouchEvent) => {

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
                oldTouches.clear()
              }
              oldTouches(t1.identifier) = t1
            }
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
                controller.scaleModel.zoomOutBy(commonX / canv.clientWidth)
                updateTouch()
              }
              else if (((nX1 < nX2) && dir1x == -1) || ((nX2 < nX1) && dir2x == -1) ||
                ((nX1 < nX2) && dir2x == 1) || ((nX2 < nX1) && dir1x == 1)) {
                controller.scaleModel.zoomInBy(commonX / canv.clientWidth)
                updateTouch()
              }
            } else if (commonY > tresh + 2) {
              if (((nY1 < nY2) && dir1y == 1) || ((nY2 < nY1) && dir2y == 1) ||
                ((nY1 < nY2) && dir2y == -1) || ((nY2 < nY1) && dir1y == -1)) {
                // zoomout
                controller.scaleModel.zoomOutBy(commonY / canv.clientHeight)
                updateTouch()
              }
              else if (((nY1 < nY2) && dir1y == -1) || ((nY2 < nY1) && dir2y == -1) ||
                ((nY1 < nY2) && dir2y == 1) || ((nY2 < nY1) && dir1y == 1)) {
                controller.scaleModel.zoomInBy(commonY / canv.clientHeight)
                updateTouch()
              }
            }
          }

        }
      case _ =>
    }
    e.preventDefault()
  })

  val touchEndListener: scala.scalajs.js.Function1[TouchEvent, Unit] = (e: TouchEvent) => {
    //oldTouches.clear()
  }

  canv.addEventListener("touchend", touchEndListener)
  canv.addEventListener("touchcangel", touchEndListener)

  def getDir(d: Double): Int = if (Math.abs(d) <= tresh) 0 else if (d < 0d) -1 else 1

  def addGeometry(obj: Object3D): Unit = {
    if (obj != null)
      scene.add(obj)
  }

  def removeGeometry(obj: Object3D): Unit = {
    if (obj != null)
      scene.remove(obj)
  }

  def onResize():Unit= if(canv.clientWidth>0){
    val w = canv.clientWidth - 1
    val h = canv.clientHeight - 1
    controller.scaleModel.setViewSize(w,h)
    renderer.setSize(w * 1.5, h * 1.5, updateStyle = true)
    renderer.domElement.style.width = w + "px"
    renderer.domElement.style.height = h + "px"
    renderer.domElement.style.overflow = "hidden"
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
    //adjustCamera()
    if (!SelectionController.supportsTouch) {
      horCross.style.width = "calc( 100% - 30px)"
      horCross.style.height = "1px"
      horCross.style.left = "0px"
      vertCross.style.width = "1px"
      vertCross.style.top = "30px"
      vertCross.style.height = "calc(100% - 60px)"
    }
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
      renderer.render(scene, camera)
    } catch {
      case e: Throwable => Log.e("render", e)
    }
    isPainting=false
  }


}
