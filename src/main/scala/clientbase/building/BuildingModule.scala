package clientbase.building

import building.{CutPlane, NoCutPlane}
import clientbase.control.{ActiveComboBox, ActiveEditField, SelectionController}
import clientbase.tableview.PluginModule
import clientbase.viewer2d.{ControllerState, MouseButtons}
import definition.data.{EMPTY_REFERENCE, OwnerReference, Reference}
import definition.expression.{NULLVECTOR, VectorConstant}
import definition.typ.SelectGroup
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{HTMLElement, Node}
import scalatags.JsDom.all._
import util.StrToDouble

import scala.scalajs.js.annotation.JSExportTopLevel

object SelectKind extends Enumeration {
  val Cells=Value("Zellen")
  val PartAreas=Value("PartArea")
  val Planes=Value("Ebenen")
}


@JSExportTopLevel("BuildingModule") class BuildingModule extends PluginModule{

  val cellInput=input(`type`:="radio",id:="cell",name:="cellorpa").render
  val cellLabel=label(`for`:="cell")("Zellen").render
  val paInput=input(`type`:="radio",id:="pa",name:="cellorpa").render
  val paLabel=label(`for`:="pa")("Teilflächen").render
  val plInput=input(`type`:="radio",id:="pl",name:="cellorpa").render
  val plLabel=label(`for`:="pl")("Ebenen").render

  val neuButton=button("Neu").render
  val viewButton=button("Ansicht").render

  val dataModel=new BuildingDataModel(this)

  val opaqueEdit=ActiveEditField.apply("text","",(tx)=> tx match{
    case StrToDouble(factor)=>
      BuildingDataModel.oqacity=factor

      dataModel.clearGeometry()
      dataModel.createGeometry()
    case o=> println("wrong factor "+o)
  })
  val headerNode: Div = div(`class` := "building-topline")(neuButton,div(style:="flex-grow:1;"),span("Auswahl:"),cellInput,cellLabel,span("  "),
    paInput,paLabel,span(" "),plInput,plLabel,div(style:="flex-grow:2;"),opaqueEdit,viewButton).render
  cellInput.checked=true

  val canvas=new BuildingCanvas(this)

  val mainNode:Div= div(`class`:="building-header")(headerNode,canvas.canvasHolder).render

  var controllerState: ControllerState.Value = ControllerState.SelectElems

  updateResize()

  override def moduleName: String = "Building"
  override def fullSize: Boolean = true
  override def content: HTMLElement = mainNode
  override def shutDown(): Unit =  dataModel.shutDown()

  override def load(ref: Reference): Unit = dataModel.load(ref)

  override def updateResize(): Unit = canvas.updateResize()

  def mouseClicked(button: MouseButtons.Value, screenx: Double, screeny: Double, controlKey: Boolean): Unit =
    button match {
      case MouseButtons.LEFT =>
        controllerState match {
          case ControllerState.SelectElems =>
            canvas.pickElems(screenx, screeny) match {
              case Some(elem)=> {
                println("Pick "+elem+" "+elem.ref)
                val newSelection = Seq(new SelectGroup(new OwnerReference(2, dataModel.buildingRef), Array(elem)))
                if (controlKey)
                  SelectionController.addSelection(newSelection, toggle = true)
                else SelectionController.select(newSelection)
              }
              case None => SelectionController.deselect()
            }
          case ControllerState.AskPoint =>
          case _ =>
        }
      case _ =>
    }

  def selectKind: SelectKind.Value =if (cellInput.checked) SelectKind.Cells else if(paInput.checked)SelectKind.PartAreas else SelectKind.Planes

  def showCutPlanes(): Unit ={
    val combo=new ActiveComboBox(NoCutPlane+:dataModel.cutPlaneSubscriber.map.values.toSeq){
      override def elemClicked(item: CutPlane): Unit ={
        dataModel.clearGeometry()
        dataModel.createGeometry(item)
      }
    }
    headerNode.appendChild(combo.elem)
  }
}
