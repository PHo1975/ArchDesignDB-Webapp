package clientbase.building

import building.{CutPlane, NoCutPlane}
import clientbase.control._
import clientbase.tableview.PluginModule
import clientbase.viewer2d.{ControllerState, MouseButtons}
import definition.data.{OwnerReference, Referencable, Reference}
import definition.expression.ObjectReference
import definition.typ.SelectGroup
import org.denigma.threejs.{Raycaster, Scene, Vector2, WebGLRenderer}
import org.scalajs.dom
import org.scalajs.dom.html._
import org.scalajs.dom.raw.{ClientRect, Event, HTMLElement, MouseEvent}
import scalatags.JsDom.all._
import util.{StrToInt, StringUtils}

import scala.scalajs.js.annotation.JSExportTopLevel


object SelectKind extends Enumeration {
  val Cells: SelectKind.Value =Value("Zellen")
  val PartAreas: SelectKind.Value =Value("PartArea")
  val Planes: SelectKind.Value =Value("Ebenen")
}

trait MyCanvas {
  var downX=0d
  var downY=0d
  var lastDragY:Double= -1
  var lastDragX:Double= -1
  var currentBounds: ClientRect = _
  val rayCaster = new Raycaster()
  val pickVector = new Vector2()

  var downButtons=0
  val canvasHolder: Div = div(`class` := "building-canvas",tabindex:="0").render
  def updateResize():Unit
  dom.window.addEventListener("resize",(e:Event)=>{
    updateResize()
  })
  canvasHolder.addEventListener("mousedown",(event:MouseEvent)=>{
    if(event.buttons==4) {lastDragX= -1;lastDragY= -1;}
    downX=event.clientX
    downY=event.clientY
    downButtons=event.buttons
  })

  canvasHolder.addEventListener("mouseup",(event:MouseEvent)=>{
    event.preventDefault()
    module.mouseClicked(MouseButtons.getButton(downButtons), downX-currentBounds.left, downY-currentBounds.top, event.ctrlKey)
  })

  def scene:Scene
  def renderer : WebGLRenderer
  def repaint():Unit
  def pickElems(screenX: Double, screenY: Double): Option[Referencable]
  def module:BuildingModule
}

trait AbstractViewModel {
  def cutPlaneChanged():Unit
  def updateData(): Unit
  def clearGeometry():Unit
}



@JSExportTopLevel("BuildingModule") class BuildingModule extends PluginModule with ObjectSelector{

  val cellInput: Input =input(`type`:="radio",id:="cell",name:="cellorpa").render
  val cellLabel: Label =label(`for`:="cell")("Zellen").render
  val paInput: Input =input(`type`:="radio",id:="pa",name:="cellorpa").render
  val paLabel: Label =label(`for`:="pa")("Teilflächen").render
  val plInput: Input =input(`type`:="radio",id:="pl",name:="cellorpa").render
  val plLabel: Label =label(`for`:="pl")("Ebenen").render
  var is3DMode:Boolean=true

  //val neuButton=button("Neu").render
  val viewButton: Button =button("Ansicht").render

  val dimensionButton:Button=button("2d").render
  val testButton:Button=button("Test").render

  val dataModel=new BuildingDataModel(this)



  val viewModel3D=new Building3DViewModel(this)
  val viewModel2D=new Building2DViewModel(this)
  var viewModel:AbstractViewModel=viewModel3D

  var selectObjectConstraints:Array[Int]=Array.empty
  var objectSelectListener:Option[ObjectSelectListener]=None

  val opaqueEdit: Input =ActiveEditField.apply("text","", {
    case StrToInt(partAreaID) =>
      viewModel3D.decoratedPartAreas.get(partAreaID) match {
        case Some(pa)=>
          val newSelection = Seq(SelectGroup(new OwnerReference(2, dataModel.buildingRef), Array(pa)))
          SelectionController.select(newSelection)
        case None =>
      }
    case o => println("wrong factor " + o)
  })
  val auswahlLabel: Span =span("Auswahl:").render
  val headerNode: Div = div(`class` := "building-topline")(testButton,dimensionButton,auswahlLabel,cellInput,cellLabel,span("  "),
    paInput,paLabel,span(" "),plInput,plLabel,div(style:="flex-grow:2;"),opaqueEdit/*,viewButton*/).render
  cellInput.checked=true

  val canvas3D=new Building3DCanvas(this)
  val canvas2D=new Building2DCanvas(this)

  var canvas:MyCanvas=canvas3D

  val mainNode:Div= div(`class`:="building-header")(headerNode,canvas.canvasHolder).render

  var controllerState: ControllerState.Value = ControllerState.SelectElems

  var currentCutPlane:CutPlane=NoCutPlane

  updateResize()

  override def moduleName: String = "Building"
  override def fullSize: Boolean = true
  override def content: HTMLElement = mainNode
  override def shutDown(): Unit = {
    dataModel.shutDown()
    SelectionController.currentObjectSelector=None
  }

  override def load(ref: Reference): Unit = {
    dataModel.load(ref)
    SelectionController.currentObjectSelector=Some(this)
    controllerState=ControllerState.SelectElems
  }

  def updateData():Unit = {
    viewModel.updateData()
    canvas.repaint()
  }

  def readyLoaded():Unit= {
    updateResize()
    showCutPlanes()
    viewModel.updateData()
    canvas.repaint()
  }

  override def updateResize(): Unit = canvas.updateResize()

  def mouseClicked(button: MouseButtons.Value, screenx: Double, screeny: Double, controlKey: Boolean): Unit =
    button match {
      case MouseButtons.LEFT =>
        controllerState match {
          case ControllerState.SelectElems =>
            canvas.pickElems(screenx, screeny) match {
              case Some(elem)=>
                println("Sel Pick "+elem+" "+elem.ref)
                val newSelection = Seq(SelectGroup(new OwnerReference(2, dataModel.buildingRef), Array(elem)))
                if (controlKey)
                  SelectionController.addSelection(newSelection, toggle = true)
                else SelectionController.select(newSelection)
              case None => SelectionController.deselect()
            }
          case ControllerState.AskObject=>
            canvas.pickElems(screenx, screeny) match {
              case Some(elem) =>
                println("Ask Pick " + elem.ref+" const:"+selectObjectConstraints.mkString(", "))
                if(selectObjectConstraints.contains(elem.ref.typ)){
                  for(l<-objectSelectListener) {
                    dataModel.updateInibitorCount=4
                    l.objectSelected(ObjectReference(elem.ref), editable = true)
                  }
                  cancelObjectSelection()
                }
              case None =>
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
        currentCutPlane=item
        viewModel.cutPlaneChanged()
        canvas.repaint()
      }
    }
    headerNode.insertBefore(combo.elem,dimensionButton)
  }

  override def askForObjectSelection(constrains: String, listener: ObjectSelectListener): Unit = {
    println("ask for Selection "+constrains)
    controllerState=ControllerState.AskObject
    selectObjectConstraints=constrains.split(",").map(StringUtils.stringToInt)
    objectSelectListener=Some(listener)
  }

  override def cancelObjectSelection(): Unit = {
    controllerState=ControllerState.SelectElems
    selectObjectConstraints=Array.empty
    objectSelectListener=None
  }

  dimensionButton.onclick= (_:MouseEvent)=>{
    dimensionButton.textContent=if(is3DMode)"3D" else "2D"
    is3DMode= !is3DMode
    mainNode.removeChild(canvas.canvasHolder)
    viewModel.clearGeometry()
    canvas=if(is3DMode)canvas3D else canvas2D
    viewModel=if(is3DMode) viewModel3D else viewModel2D
    viewModel.updateData()
    mainNode.appendChild(canvas.canvasHolder)
    canvas.updateResize()
    canvas.repaint()
  }

  testButton.onclick= (_:MouseEvent)=>{
    dimensionButton.textContent=if(is3DMode)"3D" else "2D"
    is3DMode= false
    mainNode.removeChild(canvas.canvasHolder)
    viewModel.clearGeometry()
    canvas= canvas2D
    viewModel= viewModel2D
    viewModel.clearGeometry()
    viewModel2D.createGeometry(true)
    mainNode.appendChild(canvas.canvasHolder)
    //canvas.updateResize()
    canvas.repaint()
  }


}

