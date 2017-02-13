package clientbase.viewer2d

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.Element
import org.scalajs.dom.raw._
/**
  * Created by Peter Holzer on 11.02.2017.
  */

class Viewer2DCanvas(controller:Viewer2DController,canv:Canvas,horCross:HTMLElement,vertCross:HTMLElement) {
  var isPainting=false

  var downButtons:Int=0
  var downX:Double=0
  var downY:Double=0
  var lastMouseX:Double=0
  var lastMouseY:Double=0


  dom.window.addEventListener("resize", (e:Event)=>onResize())


  val contextListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent) => {e.preventDefault()}

  canv.addEventListener("contextmenu",contextListener,false)
  horCross.addEventListener("contextmenu",contextListener,false)
  vertCross.addEventListener("contextmenu",contextListener,false)

  val wheelListener:scala.scalajs.js.Function1[WheelEvent, Unit]=(e: WheelEvent) => {
    //println("wheel "+e.deltaMode+" y:"+e.deltaY)
    if (e.deltaY < 0) controller.scaleModel.zoomPlus()
    else if (e.deltaY > 0) controller.scaleModel.zoomMinus()
    e.stopPropagation()
  }
  canv.addEventListener("mouseenter",(e:MouseEvent)=>{
    horCross.style.visibility="visible"
    vertCross.style.visibility="visible";})

  canv.addEventListener("mouseout",(e:MouseEvent)=>{
    horCross.style.visibility="hidden"
    vertCross.style.visibility="hidden"
    if(e.target==canv&&downButtons>0) mouseUpListener.apply(e)
  })

  canv.addEventListener("wheel",wheelListener,true)

  val mouseDownListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    println("click "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" "+e.ctrlKey)
    downButtons=e.buttons
    downX=e.clientX
    downY=e.clientY
    lastMouseX=e.clientX
    lastMouseY=e.clientY
    e.stopImmediatePropagation()
  }

  val moveListener:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=>{
    val rect=canv.parentElement.getBoundingClientRect()
    horCross.style.top=(e.clientY-rect.top+4).toString+"px"
    vertCross.style.left=(e.clientX-rect.left+4).toString+"px"
    if(downButtons==4){
      val dx=lastMouseX-e.clientX
      val dy=lastMouseY-e.clientY
      //println("move "+dx+" "+dy)
      controller.scaleModel.move(dx,dy)
      lastMouseX=e.clientX
      lastMouseY=e.clientY
    }
  }

  val mouseUpListener:scala.scalajs.js.Function1[MouseEvent, Unit]= (e:MouseEvent)=>{
    println("up "+e.clientX+" "+e.clientY+" b:"+e.button+" bs:"+e.buttons+" "+e.ctrlKey)
    e.stopImmediatePropagation()
    downButtons=0
  }

  canv.addEventListener("mousedown",mouseDownListener,true)
  canv.addEventListener("mouseup",mouseUpListener,true)
  canv.addEventListener("mousemove",moveListener,true)

  def onResize():Unit= if(canv.clientWidth>0){
    val par=canv.parentElement
    val w=par.clientWidth
    val h=par.clientHeight-25
    println("Resize "+w+" "+h)
    controller.scaleModel.setViewSize(w,h)
    canv.width=w*2
    canv.height=h*2
    canv.style.width=w.toString+"px"

    horCross.style.width="calc( 100% - 30px)"
    horCross.style.height="1px"
    horCross.style.left="0px"
    vertCross.style.width="1px"
    vertCross.style.top="30px"
    vertCross.style.height="calc(100% - 60px)"
    repaint()
  }

  def repaint():Unit = if(!isPainting) {
    isPainting=true
    //println("repaint clientWidth:"+canv.clientWidth+" "+canv.clientHeight+" width:"+canv.width+" "+canv.height)
    canv.getContext("2d") match {
      case ctx:CanvasRenderingContext2D =>
        ctx.scale(2,2)
        for (layer<-controller.layerList.subscriberList;if(layer.visible)){
          ctx.beginPath()
          /*val bounds=layer.bounds
          ctx.strokeStyle="yellow"
          val x1=controller.scaleModel.xToScreen(bounds.minX)
          val y1=controller.scaleModel.yToScreen(bounds.minY)
          val x2=controller.scaleModel.xToScreen(bounds.maxX)
          val y2=controller.scaleModel.yToScreen(bounds.maxY)
          ctx.moveTo(x1,y1)
          ctx.lineTo(x1,y2)
          ctx.lineTo(x2,y2)
          ctx.lineTo(x2,y1)
          ctx.lineTo(x1,y1)
          ctx.stroke()*/
          for(el<-layer.map.valuesIterator)
            el.draw(ctx,controller.scaleModel,null)
        }

      case o=> println ("Unknown Context "+o)
    }
    isPainting=false
  }
}
