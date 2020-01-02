package clientbase.viewer2d

import clientbase.connection.WebSocketConnector
import clientbase.control.{DialogManager, SelectionController}
import definition.expression.VectorConstant
import definition.typ.{AbstractCCD, AnswerDefinition, DataType, DialogQuestion}
import org.denigma.threejs.Vector3
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.{Button, Div, Select}
import org.scalajs.dom.raw.{Event, MouseEvent}
import scalatags.JsDom.all._
import org.scalajs.dom.window

class Toolbar (controller:Viewer2DController) {

  val scaleSelect: Select = select(`class` := "scales-combo").render

  lazy val neuSelect:Select = select("Neu").render

  val zoomAllBut: Button = button(onclick := { _: MouseEvent =>  controller.zoomAll() })("Alles").render
  val layerListBut: Button = button(onclick := { _: MouseEvent => controller.layerPan.toggleVisibility() })("Layer").render

  val measureSelect: Select = select("Messen").render

  val toolbarDiv: Div =div(`class` := "viewer2dbuttonbar")(if(WebSocketConnector.editable) neuSelect else div, layerListBut, zoomAllBut, if (SelectionController.supportsTouch) div() else scaleSelect,measureSelect).render

  for ((id, sc) <- ScaleModel.scales) scaleSelect.appendChild(option(attr("sid") := id.toString)(controller.scaleToString(sc)).render)
  scaleSelect.onchange = (_: Event) => scaleSelect.selectedIndex match {
    case -1 =>
    case ix => ScaleModel.scales(ix)
  }

  neuSelect.onchange= _ => {
    if(neuSelect.selectedIndex!= -1)
      createChild (SelectionController.createChildDefs (neuSelect.selectedIndex) )
    neuSelect.selectedIndex= -1
  }
  neuSelect.selectedIndex= -1

  measureSelect.onchange= _ => {
    println("measureclick "+measureSelect.selectedIndex)
    measureSelect.selectedIndex match {
      case -1 =>
      case 0 => startCoordMeasure()
      case 1 => startLengthMeasure()
      case 2 => startAreaMeasure()
    }
    measureSelect.selectedIndex = -1
  }
  measureSelect.selectedIndex = -1




  for(el<-Array("Koordinate","Strecke","Fläche"))
    measureSelect.appendChild(option/*(onclick:=measureClick)*/(el).render)

  def createChild(ccd:AbstractCCD):Unit = {
    println("create " +ccd.childName+" "+ccd.action)
    for(action<-ccd.action)
    DialogManager.startCreateAction(action,ccd.childClassID,0)
  }

  def showCreateButtons(): Unit = {
    while (neuSelect.childElementCount>0) neuSelect.removeChild(neuSelect.firstChild)
    for(ccd<-SelectionController.createChildDefs)
      neuSelect.appendChild(option(attr("sid") := ccd.childClassID.toString)(ccd.getName).render)
    neuSelect.selectedIndex= -1
  }

  def startCoordMeasure(): Unit ={

  }

  protected def formatMeasure(measure:Double): String ={
    f"${measure}%3.3f"
  }

  def startLengthMeasure(): Unit ={
    println("LengthMeasure")
    val question = DialogQuestion("Strecke messen", Seq(new AnswerDefinition("StartPunkt", DataType.VectorTyp, None)))
    val nextPointQuestion = DialogQuestion("Strecke messen", Seq(new AnswerDefinition("nächster Punkt", DataType.VectorTyp, None)), repeat = true)
    var lastPos:VectorConstant=null

    def dragger(pos:VectorConstant,context:CanvasRenderingContext2D):Unit = {
      context.fillStyle="rgb(0,0,255)"
      context.lineWidth=1
      val lastScreenPos: Vector3 =controller.toScreenPosition(lastPos.x,lastPos.y)
      controller.canvasHandler.drawLine(context,lastScreenPos.x,lastScreenPos.y,pos.x,pos.y)
    }

    DialogManager.startIntermediateQuestion(question,answerList=>{
      lastPos=answerList.last.result.toVector
      println("LastPos:"+lastPos)
      DialogManager.startIntermediateQuestion(nextPointQuestion,answerList=>{
        val endPos=answerList.last.result.toVector
        ("Distance "+(endPos-lastPos).toDouble)
        window.alert("Distance: "+formatMeasure((endPos-lastPos).toDouble)+
          "\nDX: "+formatMeasure(Math.abs(endPos.x-lastPos.x))+
          "\nDY: "+formatMeasure(Math.abs(endPos.y-lastPos.y)))
      })
      controller.setCustomDragger(dragger)
    })
  }

  def startAreaMeasure():Unit = {

  }


}
