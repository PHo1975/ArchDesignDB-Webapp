package clientbase.building

import clientbase.viewer2d.MouseButtons
import definition.data.Referencable
import org.denigma.threejs._
import org.denigma.threejs.extras.OrbitControls
import org.scalajs.dom
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{ClientRect, Event, HTMLElement, MouseEvent, WheelEvent}
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.{Log, StrToInt}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal


class BuildingCanvas(module:BuildingModule) {
  val whiteColor = new Color(0.95d, 0.95d, .95d)
  val camera = new PerspectiveCamera(50,4.0/3.0, 0.1, 1000)
  var lastDragY:Double= -1
  var lastDragX:Double= -1
  val center=new Vector3(0,0,0)
  var canvasWidth:Int=0
  var canvasHeight:Int=0
  val rayCaster = new Raycaster()
  val pickVector = new Vector2()
  var downX=0d
  var downY=0d
  var downButtons=0
  var currentBounds: ClientRect = _

  var theta=Math.PI/4d
  var phi=0d
  var radius=40d

  camera.position.x= 0
  //camera.position.y= radius*Math.cos(theta)
  //camera.position.z= -Math.sin(theta)*radius
  camera.position.z=radius*Math.cos(theta)
  camera.position.y= -Math.sin(theta)*radius

  camera.lookAt(center)
  camera.updateProjectionMatrix()

  camera.up.y=0d
  camera.up.z=1d


  val scene=new Scene()
  val axis=new AxesHelper()
  axis.scale.x= -2f
  axis.scale.y=2f
  axis.scale.z=2f
  axis.updateMatrixWorld()
  scene.add(axis)

  val renderer = new WebGLRenderer(js.Dynamic.literal(alpha = true, clearColor = whiteColor, autoSize = false, antialias = true,devicePixelratio=window.devicePixelRatio
  ).asInstanceOf[WebGLRendererParameters])
  //renderer.domElement.style.position="absolute"
  renderer.domElement.style.overflow = "hidden"
  renderer.domElement.style.border="1px black solid"

  val canvasHolder: Div = div(`class` := "building-canvas",tabindex:="0").render
  canvasHolder.appendChild(renderer.domElement)


  dom.window.addEventListener("resize",(e:Event)=>{
    updateResize()
  })

  canvasHolder.addEventListener("wheel",(event:WheelEvent)=>{
    if (event.deltaY<0 ) radius=radius*.9
    else if(event.deltaY>1) radius=radius*1.1
    updateRender()
  })

  canvasHolder.addEventListener("mousedown",(event:MouseEvent)=>{
    if(event.buttons==4) {lastDragX= -1;lastDragY= -1;}
    downX=event.clientX
    downY=event.clientY
    downButtons=event.buttons
  })

  canvasHolder.addEventListener("mousemove",(event:MouseEvent)=>{
    if(event.buttons==4){
      if(lastDragX!= -1) {
        val deltax=event.clientX-lastDragX
        val angle=deltax*Math.PI/0.8/renderer.domElement.clientWidth
        phi+=angle
      }
      if(lastDragY!= -1) {
        val deltay=event.clientY-lastDragY
        val angle=deltay*Math.PI/2/renderer.domElement.clientHeight
        theta-=angle
        if(theta<0.001d) theta=0.001d
      }

      lastDragY=event.clientY
      lastDragX=event.clientX
      updateRender()
    }
  },useCapture = true)

  canvasHolder.addEventListener("mouseup",(event:MouseEvent)=>{
    event.preventDefault()
    module.mouseClicked(MouseButtons.getButton(downButtons), downX-currentBounds.left, downY-currentBounds.top, event.ctrlKey)
  })


  def updateRender(): Unit = {
    camera.position.x= -radius*Math.sin(phi)
    //camera.position.y= radius*Math.cos(theta)
    //camera.position.z= -Math.sin(theta)*radius*Math.cos(phi)
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
    implicit val ordering=Ordering.Double.TotalOrdering
    val intersects: js.Array[Intersection] = rayCaster.intersectObjects(scene.children).filter(_.`object`.name.length>0).sortBy(_.distance)
    println("Pickall:"+intersects.map(_.`object`.name).mkString(" | " ))
    if(intersects.nonEmpty) intersects.minBy(_.distance).`object`.name match{
      case StrToInt(id)=> //println("PartArea "+id);
        val partArea=module.dataModel.partAreaSubscriber.map(id)
        module.selectKind match {
          case SelectKind.Cells=>Some(partArea.firstCell)
          case SelectKind.PartAreas=>
            //partArea.showSelection()
            //repaint()
            Some(partArea)
          case SelectKind.Planes=>Some(partArea.defPlane)
        }
      case o=>Log.e("Unknown PartArea Name "+o);None
    }
    else None
  }
}
