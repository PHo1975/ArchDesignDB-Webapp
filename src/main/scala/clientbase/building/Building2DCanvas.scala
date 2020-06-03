package clientbase.building

import definition.data.Referencable
import org.denigma.threejs._
import org.scalajs.dom.raw.{MouseEvent, WheelEvent}
import org.scalajs.dom.window
import util.{Log, StrToInt}

import scala.math.Ordering.Double
import scala.scalajs.js

class Building2DCanvas(val module:BuildingModule) extends MyCanvas {
  val whiteColor = new Color(0.95d, 0.95d, .95d)
  val camera = new OrthographicCamera(-50,50,50,-50,1,1000)
  val scene=new Scene()
  var canvasWidth:Int=0
  var canvasHeight:Int=0
  val borderWidth=0.5d
  var screenCenterX:Double=0
  var screenCenterY:Double=0
  var worldWidth:Double=0
  var worldHeight:Double=0
  var zoom=1d
  //var frustumSize=20

  val renderer = new WebGLRenderer(js.Dynamic.literal(alpha = true, clearColor = whiteColor, autoSize = false, antialias = true,devicePixelratio=window.devicePixelRatio
  ).asInstanceOf[WebGLRendererParameters])
  //renderer.domElement.style.position="absolute"
  renderer.domElement.style.overflow = "hidden"
  renderer.domElement.style.border="1px black solid"

  canvasHolder.appendChild(renderer.domElement)


  def updateResize(): Unit =if(canvasHolder.clientWidth>0){
    currentBounds = canvasHolder.getBoundingClientRect()
    canvasHeight=canvasHolder.clientHeight-1
    canvasWidth=canvasHolder.clientWidth-1
    renderer.domElement.width=canvasWidth
    renderer.domElement.height=canvasHeight
    renderer.setSize(canvasWidth,canvasHeight,updateStyle = false)
    val aspect=canvasWidth.toDouble/canvasHeight.toDouble
    val modelWidth=module.viewModel2D.maxX-module.viewModel2D.minX
    val modelHeight=module.viewModel2D.maxY-module.viewModel2D.minY
    zoom=1d
    lastDragX= -1
    lastDragY= -1
    if(module.viewModel2D.maxX==Float.MinValue.toDouble) {
      val frustumSize=20d
      camera.left= -0.5*frustumSize*aspect
      camera.right= 0.5*frustumSize*aspect
      camera.top= frustumSize/2
      camera.bottom= -frustumSize/2
      screenCenterX=0
      screenCenterY=0
    }
    else {
      screenCenterX=(module.viewModel2D.maxX + module.viewModel2D.minX) / 2
      screenCenterY = (module.viewModel2D.minY + module.viewModel2D.maxY) / 2
      val modelAspect = modelWidth / modelHeight
      if (modelAspect < aspect) { //fit to modelHeight
        println("Fit to height ma:"+modelAspect+" mh:"+modelHeight)
        camera.top = module.viewModel2D.maxY + borderWidth
        camera.bottom = module.viewModel2D.minY - borderWidth
        val frustrumHeight: Double = modelHeight + borderWidth * 2
        worldHeight=frustrumHeight
        worldWidth=frustrumHeight*aspect
        camera.left = screenCenterX - frustrumHeight * aspect*0.5
        camera.right = screenCenterX + frustrumHeight * aspect*0.5
      } else { // fit to modelWidth
        camera.left = module.viewModel2D.minX - borderWidth
        camera.right = module.viewModel2D.maxX + borderWidth
        val frustumWidth: Double = modelWidth + borderWidth * 2
        worldWidth=frustumWidth
        worldHeight=frustumWidth/aspect
        camera.top = screenCenterY + frustumWidth / aspect*0.5
        camera.bottom = screenCenterY - frustumWidth / aspect*0.5
      }
    }
    camera.updateProjectionMatrix()
    repaint()
  }

  canvasHolder.addEventListener("mousemove",(event:MouseEvent)=>{
    if(event.buttons==4)
      updateDrag(event.clientX,event.clientY)
  },useCapture = true)

  canvasHolder.addEventListener("wheel",(event:WheelEvent)=>{
    if (event.deltaY<0 ) zoom=zoom*0.9
    else if(event.deltaY>1) zoom=zoom/0.9
    updateRender()
  })

  protected def updateDrag(clientX:Double,clientY:Double): Unit ={
    if(lastDragX!= -1) {
      val deltax=lastDragX-clientX
      val canvasWidth=canvasHolder.clientWidth-1
      screenCenterX+=deltax*worldWidth/canvasWidth*zoom
    }
    if(lastDragY!= -1) {
      val deltay=clientY-lastDragY
      val canvasHeight=canvasHolder.clientHeight-1
      screenCenterY+=deltay*worldHeight/canvasHeight*zoom
    }
    lastDragY=clientY
    lastDragX=clientX
    updateRender()
  }

  def updateRender(): Unit = {
    camera.left=screenCenterX-worldWidth/2*zoom
    camera.right=screenCenterX+worldWidth/2*zoom
    camera.top=screenCenterY+worldHeight/2*zoom
    camera.bottom=screenCenterY-worldHeight/2*zoom
    camera.updateProjectionMatrix()
    repaint()
  }


  def repaint(): Unit =  renderer.render(scene,camera)
  def pickElems(screenX: Double, screenY: Double): Option[Referencable]={

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
      case StrToInt(id)=>
        //println("PartArea "+id);
        module.viewModel2D.getLine(id)
      case o=>Log.e("Unknown PartArea Name "+o);None
    }
    else None
  }
}
