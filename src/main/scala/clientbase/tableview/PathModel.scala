package clientbase.tableview

import clientbase.connection.{InstSubscriber, WebSocketConnector}
import clientbase.control.{FocusContainer, SelectionController}
import definition.data._
import definition.typ.AllClasses
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import util.Log

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Peter Holzer on 09.08.2015.
 */
class PathModel(parentNode:HTMLElement,contentNode:HTMLElement) extends InstSubscriber with FocusContainer with Referencable {

  protected val propList: ArrayBuffer[PropertyModel] =collection.mutable.ArrayBuffer[PropertyModel]()
  var oldData:Seq[InstanceData]=Seq.empty
  var lastFocusedTable:Option[TableModel]=None
  var ref:Reference=EMPTY_REFERENCE
  var currentModule:Option[PluginModule]=None

  override def onLoad(data: Iterator[InstanceData]): Unit = {
    oldData=data.toIndexedSeq
    //println("Path on load:"+oldData.mkString("|"))
    intUpdate()
  }

  protected def intUpdate():Unit= {
    clearPath()
    val topElement=oldData.last
    document.title=topElement.toString()
    for(i<-oldData.indices;pathData=oldData(i)) {
      parentNode.appendChild(createPathButton(pathData, i, pathData == topElement))
      if(pathData!=topElement)parentNode.appendChild(p(`class`:="pathbut-divider")(">").render)
    }
    loadPropFields(topElement.ref)
    dom.window.history.pushState(topElement.ref.bToString(),topElement.toString,"?"+topElement.ref.bToString)
  }

  protected def loadPropFields(topRef:Reference):Unit = {
    SelectionController.deselect()
    ref=topRef
    println("load ref "+ref)
    if(PluginModules.contains(ref.typ)) {
      val module=PluginModules(ref.typ)
      println("load Module "+module)
      contentNode.appendChild(module.content)
      module.load(ref)
      currentModule=Some(module)
    } else {
      currentModule=None
      val prFields = AllClasses.get.getClassByID(topRef.typ).propFields

      def loop(prIx: Int): Unit = if (prIx < prFields.size) {
        val propField = prFields(prIx)
        if (!propField.hidden) {
          val createClasses = propField.createChildDefs.filter(a => a.editorName == "" && a.action.isEmpty)
          val rowDiv = div(overflow := "hidden", clear := "both").render
          rowDiv.appendChild(p(`class` := "prfieldtitle")(propField.name + ":").render)
          if (createClasses.nonEmpty) {
            def clearButtons(): Unit = {
              val numChild = rowDiv.childElementCount
              for (_ <- 2 until numChild)
                rowDiv.removeChild(rowDiv.children(2))
            }

            def showNewClasses(): Unit = {
              clearButtons()
              for (cr <- createClasses)
                rowDiv.appendChild(button(`class` := "prfield-createbutton", onclick := { () => {
                  WebSocketConnector.createInstance(cr.childClassID, Array(new OwnerReference(prIx, topRef)))
                  clearButtons()
                }
                })(cr.childName).render)
            }

            if (WebSocketConnector.editable) rowDiv.appendChild(button(`class` := "prfield-createbutton", onclick := { () => showNewClasses() })("Neu...").render)
          }
          contentNode.appendChild(rowDiv)
          val divNode = div(`class` := "prfielddiv").render
          contentNode.appendChild(divNode)
          val propModel = new PropertyModel(topRef, prIx, contentNode, subsID, () => {
            loop(prIx + 1)
          }, propField.single, propField.allowedClass, this)
          propList += propModel
          WebSocketConnector.createSubscription(topRef, prIx, propModel)
        } else loop(prIx + 1)
      } //else Log.w("path ready loaded")
      loop(0)
    }
  }

  override def onUpdate(data: Iterator[InstanceData]): Unit = {
    println("on update ")
    clearPath()
    onLoad(data)
  }

  override def onChange(data: InstanceData): Unit = {
    oldData.indexWhere(_.ref==data.ref) match{
      case -1=> Log.e("Path onChange cant find "+data.ref)
      case ix=> parentNode.replaceChild(createPathButton(data,ix,data.ref==oldData.last.ref),
        parentNode.childNodes.item(ix))
    }
  }

  override def onDelete(data: Reference): Unit = {}

  override def onChildAdded(data: InstanceData): Unit = {}

  def clearPath(): Unit = {
    while(parentNode.hasChildNodes()) parentNode.removeChild(parentNode.firstChild)
    while(contentNode.hasChildNodes()) contentNode.removeChild(contentNode.firstChild)
    for(pr<-propList) pr.shutDown()
    for(mod<-currentModule)mod.shutDown()
    propList.clear()
  }


  protected def createPathButton(data:InstanceData,indent:Int,last:Boolean): Paragraph =
    if (last)
       p(`class`:="pathbut-last")(data.toString).render
    else
      p(`class`:="pathbut",onclick:={()=>jumpUp(indent)})(data.toString).render


  protected def jumpUp(indent:Int):Unit={
    println("Jump up "+indent+" olddata:"+oldData.mkString("|"))
    if(indent<oldData.size){
      WebSocketConnector.pathJumpUp(subsID,indent)
      oldData=oldData.take(indent+1)
      println("changed:"+oldData.mkString("|"))
      intUpdate()
    }
  }

  def containerFocused(atable:Option[TableModel],curPropertyField:Int):Unit = {
    atable match {
      case a @ Some(_) => lastFocusedTable = a
      case _ =>
    }
    SelectionController.containerFocused(this,curPropertyField)
  }

  override def containerName: String = ""

  override def getOwnerRef: Option[Referencable] = Some(this)

  override def requestFocus(): Unit = lastFocusedTable match {
    case Some(atable)=> atable.requestFocus()
    case _=>
  }

  def updateResize(): Unit =for(m<-currentModule) m.updateResize()
}
