package clientbase.building

import clientbase.viewer2d.MouseButtons
import definition.data.Referencable
import org.denigma.threejs._
import org.scalajs.dom.raw.{MouseEvent, TouchEvent, TouchList, WheelEvent}
import org.scalajs.dom.window
import util.{Log, StrToInt}

import scala.math.Ordering.Double
import scala.scalajs.js


class Building3DCanvas(val module:BuildingModule) extends MyCanvas {
  val whiteColor = new Color(0.95d, 0.95d, .95d)
  val camera = new PerspectiveCamera(50,4.0/3.0, 0.1, 1000)
  val center=new Vector3(0,0,0)
  var canvasWidth:Int=0
  var canvasHeight:Int=0

  var theta=Math.PI/4d
  var phi=0d
  var radius=40d
  var oldRadius=0d
  var oldTouches:TouchList=_

  camera.position.x= 0
  camera.position.z=radius*Math.cos(theta)
  camera.position.y= -Math.sin(theta)*radius
  camera.lookAt(center)
  camera.updateProjectionMatrix()
  camera.up.y=0d
  camera.up.z=1d

  val scene=new Scene()

  val renderer = new WebGLRenderer(js.Dynamic.literal(alpha = true, clearColor = whiteColor, autoSize = false, antialias = true,devicePixelratio=window.devicePixelRatio
  ).asInstanceOf[WebGLRendererParameters])

  renderer.domElement.style.overflow = "hidden"
  renderer.domElement.style.border="1px black solid"

  canvasHolder.appendChild(renderer.domElement)
  canvasHolder.addEventListener("wheel",(event:WheelEvent)=>{
    if (event.deltaY<0 ) radius=radius*.9
    else if(event.deltaY>1) radius=radius*1.1
    updateRender()
  })

   protected def updateDrag(clientX:Double,clientY:Double): Unit ={
    if(lastDragX!= -1) {
      val deltax=clientX-lastDragX
      val angle=deltax*Math.PI/0.8/renderer.domElement.clientWidth
      phi+=angle
    }
    if(lastDragY!= -1) {
      val deltay=clientY-lastDragY
      val angle=deltay*Math.PI/2/renderer.domElement.clientHeight
      theta-=angle
      if(theta<0.001d) theta=0.001d
    }

    lastDragY=clientY
    lastDragX=clientX
    updateRender()
  }


  canvasHolder.addEventListener("mousemove",(event:MouseEvent)=>{
    if(event.buttons==4)
      updateDrag(event.clientX,event.clientY)
  },useCapture = true)

  canvasHolder.addEventListener("touchstart",(event:TouchEvent)=>{
    event.targetTouches.length match {
      case 1=>
        lastDragX= -1;lastDragY= -1
        val touch=event.targetTouches(0)
        downX=touch.clientX
        downY=touch.clientY
      case 2=>
        lastDragX= -1;lastDragY= -1
        oldTouches=event.targetTouches
        oldRadius=radius
      case _=>
    }
    event.preventDefault()
    event.stopPropagation()
  },useCapture = true)

  private def touchDistance(t:TouchList): Double ={
    val t1=t(0)
    val t2=t(1)
    val dx=t2.clientX-t1.clientX
    val dy=t2.clientY-t1.clientY
    Math.sqrt(dx*dx+dy*dy)
  }


  canvasHolder.addEventListener("touchmove",(event:TouchEvent)=>{
    event.targetTouches.length match {
      case 1=>
        val touch=event.targetTouches(0)
        updateDrag(touch.clientX,touch.clientY)
      case 2=>
        val oldDistance: Double =touchDistance(oldTouches)
        val newDistance: Double =touchDistance(event.targetTouches)
        println("oldDistance:"+oldDistance+" newDistance:"+newDistance)
        radius=oldRadius*oldDistance/newDistance
        updateRender()
      case _=>
    }
    event.preventDefault()
    event.stopPropagation()
  },useCapture = true)

  canvasHolder.addEventListener("touchend",(event:TouchEvent)=>{

    event.changedTouches.length match {
      case 1=>
        val touch=event.changedTouches(0)
        println("touchend oldPos:"+downX+" ,"+downY+" newpos:"+touch.clientX+" , "+touch.clientY)
        if(Math.abs(downX-touch.clientX)<2 && Math.abs(downY-touch.clientY)<2)
          module.mouseClicked(MouseButtons.LEFT, downX-currentBounds.left, downY-currentBounds.top, controlKey = false)
      case _ =>
    }
    event.preventDefault()
    event.stopPropagation()
  },useCapture = true)

  canvasHolder.addEventListener("touchcancel",(event:TouchEvent)=>{

  })


  def updateRender(): Unit = {
    camera.position.x= -radius*Math.sin(phi)
    camera.position.y= -Math.sin(theta)*radius*Math.cos(phi)
    camera.position.z= radius*Math.cos(theta)

    camera.lookAt(center)
    camera.updateProjectionMatrix()
    renderer.render(scene,camera)
  }

  def repaint(): Unit =  renderer.render(scene,camera)

  def updateResize(): Unit =if(canvasHolder.clientWidth>0){
    currentBounds = canvasHolder.getBoundingClientRect()
    canvasHeight=canvasHolder.clientHeight-1
    canvasWidth=canvasHolder.clientWidth-1
    renderer.domElement.width=canvasWidth
    renderer.domElement.height=canvasHeight
    renderer.setSize(canvasWidth,canvasHeight,updateStyle = false)
    camera.aspect=canvasWidth.toDouble/canvasHeight.toDouble
    camera.updateProjectionMatrix()
    repaint()
  }

  def pickElems(screenX: Double, screenY: Double): Option[Referencable] = {
    //println("Pick Elements "+screenX+" , "+screenY)
    val viewBonds = renderer.domElement.getBoundingClientRect()
    var testx=0d
    var testy=0d
    testx=screenX+viewBonds.left;testy=screenY+viewBonds.top
    pickVector.x = ((testx - viewBonds.left) / viewBonds.width) * 2d - 1d
    pickVector.y = 1d - ((testy - viewBonds.top) / viewBonds.height) * 2d
    rayCaster.setFromCamera(pickVector, camera)
    rayCaster.linePrecision=0.2d
    implicit val ordering: Double.TotalOrdering.type =Ordering.Double.TotalOrdering
    val intersects: js.Array[Intersection] = rayCaster.intersectObjects(scene.children).filter(_.`object`.name.length>0)
    //println("Pickall:"+intersects.map(el=>el.`object`.name+"="+el.distance).mkString(" | " ))
    if(intersects.nonEmpty) intersects.minBy(_.distance).`object`.name match{
      case BuildingDataModel.planeMatcher(StrToInt(id))=>
        Some(module.viewModel3D.decoratedPlanes(id))
      case StrToInt(id)=>
        //println("PartArea "+id);
        val decoratedPartArea=module.viewModel3D.decoratedPartAreas(id)
        module.selectKind match {
          case SelectKind.Cells=>Some(module.dataModel.getCell(decoratedPartArea.pa.firstCellID))
          case SelectKind.PartAreas=>  Some(decoratedPartArea)
          case SelectKind.Planes=>Some(decoratedPartArea.pa.defPlane)
        }
      case o=>Log.e("Unknown PartArea Name "+o);None
    }
    else None
  }
}
