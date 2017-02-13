package clientbase.tableview

import clientbase.connection.{InstSubscriber, Subscriber, WebSocketConnector}
import clientbase.control.{SelectionController, SidepanelController}
import definition.data.{InstanceData, OwnerReference, Reference}
import definition.typ.AllClasses
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLDocument, HTMLElement}
import util.Log
import org.scalajs.dom.document
import org.scalajs.dom.html.Paragraph

import scalatags.JsDom.all._

/**
 * Created by Peter Holzer on 09.08.2015.
 */
class PathModel(parentNode:HTMLElement,contentNode:HTMLElement) extends InstSubscriber {

  protected val propList=collection.mutable.ArrayBuffer[PropertyModel]()
  var oldData:Seq[InstanceData]=Seq.empty

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
    val prFields=AllClasses.get.getClassByID(topRef.typ).propFields
    def loop(prIx:Int):Unit= if(prIx<prFields.size){
      val propField=prFields(prIx)
      if(!propField.hidden) {
        val createClasses=propField.createChildDefs.filter(a=>a.editorName==""&&a.action.isEmpty)
        val rowDiv=div(`style`:="overflow:hidden;clear:both;").render
        rowDiv.appendChild(p(`class`:="prfieldtitle")(propField.name+":").render)
        if(createClasses.nonEmpty){
          def clearButtons()={
            val numChild=rowDiv.childElementCount
            for(i<-2 until numChild)
              rowDiv.removeChild(rowDiv.children(2))
          }
          def showNewClasses()={
            clearButtons()
            for(cr<-createClasses)
              rowDiv.appendChild(button(`class`:="prfield-createbutton",onclick:={()=>{
                WebSocketConnector.createInstance(cr.childClassID,Array(new OwnerReference(prIx,topRef)),(_)=>{})
                clearButtons()
              }})(cr.childName).render)
          }
          rowDiv.appendChild(button(`class`:="prfield-createbutton",onclick:={()=>showNewClasses})("Neu...").render)
        }
        contentNode.appendChild(rowDiv)
        val divNode=div(`class`:="prfielddiv").render
        contentNode.appendChild(divNode)
        val propModel=new PropertyModel(topRef,prIx,contentNode,subsID,()=>{loop(prIx+1)},propField.single,propField.allowedClass)
        propList+=propModel
        WebSocketConnector.createSubscription(topRef,prIx,propModel)
      } else loop(prIx+1)
    } //else Log.w("path ready loaded")

    loop(0)


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

  protected def clearPath(): Unit = {
    while(parentNode.hasChildNodes()) parentNode.removeChild(parentNode.firstChild)
    while(contentNode.hasChildNodes()) contentNode.removeChild(contentNode.firstChild)
    for(pr<-propList) pr.shutDown()
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
}
