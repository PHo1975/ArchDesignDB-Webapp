package clientbase.control

import org.scalajs.dom.window
import org.scalajs.dom.raw._

import scala.scalajs.js.annotation.JSExport
/**
 * Created by Peter Holzer on 24.09.2015.
 */
abstract class LongTouchControl(elem:HTMLElement)  {
  var timerHandle:Int=0
  var timerUsed=false
  //var touches:TouchList

  def touchStart(e:TouchEvent):Unit= if(!timerUsed){
    timerHandle=window.setTimeout(()=>onLongTouch() , 500d)
    timerUsed=true
    //touches=e.touches
    //e.preventDefault()
    //e.stopPropagation()
  }

  def timeTriggered():Unit={
    onLongTouch()
    timerUsed=false
    if(timerHandle!=0)window.clearTimeout(timerHandle)
    timerHandle=0
  }

  def touchEnd(e:Event):Unit= if(timerUsed) {
    timerUsed =false
    window.clearTimeout(timerHandle)
    timerHandle=0
  }

  def touchMove(e:TouchEvent):Unit= {

  }

  def onLongTouch():Unit={println("Hello")}

  def setup():Unit= {
    elem.addEventListener("touchstart", touchStart _, useCapture = false)
    elem.addEventListener("touchend", touchEnd _, useCapture =false)
    elem.addEventListener("touchmove", touchEnd _, useCapture =false)
    elem.addEventListener("touchcancel", touchEnd _, useCapture =false)
    elem.addEventListener("contextmenu", (e: Event) => {e.preventDefault(); e.stopPropagation();}, useCapture =true)

    elem.addEventListener("dragleave", touchEnd _, useCapture =false)
    elem.addEventListener("focusout", touchEnd _, useCapture =false)
    elem.addEventListener("mouseup", touchEnd _, useCapture =false)
  }
}
