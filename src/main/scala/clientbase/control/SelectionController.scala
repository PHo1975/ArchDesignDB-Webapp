package clientbase.control

import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.SelectionDecorable
import definition.data.{OwnerReference, Referencable, Reference}
import definition.typ.{AbstractObjectClass, AllClasses, SelectGroup}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by Peter Holzer on 20.09.2015.
 */


object SelectionController {
  val currentSelection:ArrayBuffer[SelectGroup[_ <: Referencable]]=ArrayBuffer.empty

  val cellEditor=new CellEditor

  var focusedElement:Option[FocusOwner]=None

  var commonClass:Option[AbstractObjectClass]=None

  lazy val supportsTouch: Boolean = org.scalajs.dom.window.hasOwnProperty("ontouchstart")

  def setFocusedElement(owner: FocusOwner): Unit = if (focusedElement.isEmpty || focusedElement.get != owner) {
    println("setFocusedElement "+owner+" focusedElem:"+focusedElement)
    for(f<-focusedElement)f.blur()
    focusedElement=Some(owner)
  }

  def select(selection: Seq[SelectGroup[_ <: Referencable]]): Unit = {
    cellEditor.finishEdit(0, focusTable = false)
    resetOldSelection()
    currentSelection.clear
    for(ngroup<-selection) {
      val list=  getOrCreateChildList(ngroup.parent)
      list++= ngroup.children
    }
    decorateNewSelection()
    updateCommonClassID()
  }

  protected def updateCommonClassID():Unit= {
    val commonClassID=AllClasses.get.getCommonClassForGroups(currentSelection)
    commonClass= if(commonClassID>0 ) Some(AllClasses.get.getClassByID(commonClassID)) else None
    notifySelectionChanged()
  }


  def addSelection(selection: Seq[SelectGroup[_ <: Referencable]],toggle:Boolean): Unit = {
    cellEditor.finishEdit(0, focusTable = false)

    for(ngroup<-selection;if ngroup.children.nonEmpty) {
      val list=getOrCreateChildList(ngroup.parent)
      if (toggle) {
        for (nel<-ngroup.children){
          val elix=list.indexOf(nel)
          if(elix < 0) {
            list+=nel
            decorateElem(nel)
          }
          else{
            list.remove(elix)
            unDecorateElem(nel)
          }
        }
      } else {
        for (nel<-ngroup.children)
          if(!list.contains(nel)){
            list+=nel
            decorateElem(nel)
          }
      }
    }
    updateCommonClassID()
  }

  protected def getOrCreateChildList(ref:OwnerReference):ArrayBuffer[Referencable] ={
    val oldIx = currentSelection.indexWhere(_.parent == ref)
    if(oldIx == -1) {
      val newList=new ArrayBuffer[Referencable]()
      val newGroup=new SelectGroup[Referencable](ref,newList)
      currentSelection += newGroup
      newList
    } else currentSelection(oldIx).children.asInstanceOf[ArrayBuffer[Referencable]]
  }

  protected def decorateElem(el:Referencable): Unit =el match {
    case dec: SelectionDecorable => dec.showSelection()
    case _ =>
  }

  protected def unDecorateElem(el:Referencable): Unit =el match {
    case dec: SelectionDecorable => dec.hideSelection()
    case _ =>
  }

  def containsSelection(selection: Iterable[SelectGroup[_ <: Referencable]]): Unit = {

  }

  def resetOldSelection(): Unit =
    for (cgroup ← currentSelection; elem ← cgroup.children) elem match {
      case dec: SelectionDecorable ⇒ dec.hideSelection()
      case _ ⇒
    }

  def decorateNewSelection(): Unit =
    for (cgroup ← currentSelection; elem ← cgroup.children) elem match {
      case dec: SelectionDecorable ⇒ dec.showSelection()
      case _ ⇒
    }


  def deselect(): Unit = {
    cellEditor.finishEdit(0, focusTable = false)
    resetOldSelection()
    currentSelection.clear
    commonClass=None
    notifySelectionChanged()
  }

  protected def notifySelectionChanged(): Unit = if (WebSocketConnector.editable) {
    val numElems=currentSelection.foldLeft(0)((sum,el)=>sum+el.children.size)
    commonClass match {
      case Some(c) =>
        SidepanelController.setSelection("" + numElems + " " +
          c.getDescriptionOrName + (if (numElems == 1) " -Objekt" else " -Objekte"+"<br>"+c.fieldEditors.mkString(", ")), c.actions)
        FieldEditorPanel.loadFieldEditors(c,currentSelection)
      case None =>
        SidepanelController.setSelection(" - ",mutable.LinkedHashMap.empty)
        FieldEditorPanel.clearFieldEditors()
    }
  }

  def printMessage(text: String): Unit = SidepanelController.messageLabel.innerHTML = text

  def elementChanged(parent:Reference,child:Referencable):Unit = {
    val groupIx=currentSelection.indexWhere(_.parent.ownerRef==parent)
    if(groupIx >=0) {
      val groupList=currentSelection(groupIx).children.asInstanceOf[ArrayBuffer[Referencable]]
      val elIx=groupList.indexWhere(_.ref==child.ref)
      if(elIx>= 0 ) {
        groupList(elIx)=child
        decorateElem(child)
      }
    }
  }

  def elementRemoved(parent:Reference,child: Referencable):Unit = if(currentSelection.nonEmpty){
    val groupIx=currentSelection.indexWhere(_.parent.ownerRef==parent)
    if(groupIx >=0) {
      val groupList=currentSelection(groupIx).children.asInstanceOf[ArrayBuffer[Referencable]]
      val elIx=groupList.indexWhere(_.ref==child.ref)
      if(elIx>= 0 ) groupList.remove(elIx)
      updateCommonClassID()
    }
  }

  def ownerInvisible(owner:Reference):Unit = {
    for(ix <-currentSelection.indices;cgroup=currentSelection(ix);if cgroup.parent.ownerRef==owner)
      currentSelection.remove(ix)
    updateCommonClassID()
  }


}
