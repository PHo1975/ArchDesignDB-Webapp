package clientbase.viewer2d

import clientbase.connection.WebSocketConnector
import clientbase.control.SelectionController
import definition.typ.{AbstractCCD, AnswerDefinition, DataType, DialogQuestion}
import org.scalajs.dom.html.{Button, Div, Select}
import org.scalajs.dom.raw.{Event, MouseEvent}
import scalatags.JsDom.all._

class Toolbar (controller:Viewer2DController) {

  val scaleSelect: Select = select(`class` := "scales-combo").render

  lazy val neuSelect:Select = select(onchange := {e:Event  => {
    createChild(SelectionController.createChildDefs(neuSelect.selectedIndex))
  }
  })("Neu").render

  val zoomAllBut: Button = button(onclick := { _: MouseEvent =>  controller.zoomAll() })("Alles").render
  val layerListBut: Button = button(onclick := { _: MouseEvent => controller.layerPan.toggleVisibility() })("Layer").render
  val measureSelect: Select = select(onchange := { e:Event => measureSelect.selectedIndex match {
    case -1 =>
    case 0 => startCoordMeasure()
    case 1 => startLengthMeasure()
    case 2 => startAreaMeasure()
  }  })("Messen").render

  val toolbarDiv: Div =div(`class` := "viewer2dbuttonbar")(if(WebSocketConnector.editable) neuSelect else div, layerListBut, zoomAllBut, if (SelectionController.supportsTouch) div() else scaleSelect,measureSelect).render

  for ((id, sc) <- ScaleModel.scales) scaleSelect.appendChild(option(attr("sid") := id.toString)(controller.scaleToString(sc)).render)
  scaleSelect.onchange = (_: Event) => scaleSelect.selectedIndex match {
    case -1 =>
    case ix => ScaleModel.scales(ix)
  }

  for(el<-Array("Koordinate","Strecke","Fläche"))
    measureSelect.appendChild(option(el).render)

  def createChild(ccd:AbstractCCD):Unit = {
    println("create " +ccd.childName+" "+ccd.action)
  }

  def showCreateButtons(): Unit = {
    while (neuSelect.childElementCount>0) neuSelect.removeChild(neuSelect.firstChild)
    for(ccd<-SelectionController.createChildDefs) neuSelect.appendChild(option(attr("sid") := ccd.childClassID.toString)(ccd.getName).render)
  }

  def startCoordMeasure(): Unit ={

  }

  def startLengthMeasure(): Unit ={
    println("LengthMeasure")
    val question = DialogQuestion("Strecke messen", Seq(new AnswerDefinition("StartPunkt", DataType.VectorTyp, None)))

  }

  def startAreaMeasure():Unit = {

  }


}
