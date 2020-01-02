package clientbase.control

import clientbase.connection.{WebObjectClass, WebSocketConnector}
import clientbase.viewer2d.{Formatable, SelectionDecorable}
import definition.data.{EMPTY_OWNERREF, OwnerReference, Referencable, Reference}
import definition.expression.Constant
import definition.typ.{AbstractCCD, AbstractObjectClass, AllClasses, SelectGroup}

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

  var lastContainer:Option[FocusContainer]=None
  var lastOwnerRef:Option[Referencable]=None
  var lastPropField:Int= -1
  var createChildDefs:Seq[AbstractCCD]=Seq.empty

  val lastFormatableSelection: mutable.HashMap[Int, Formatable] = collection.mutable.HashMap[Int, Formatable]()

  protected def storeLastFormatables():Unit =
    for(group<-currentSelection;el<-group.children)
      storeFormatable(el)


  protected def storeFormatable(el:Referencable):Unit=
    el match {
      case fo:Formatable=> lastFormatableSelection(fo.ref.typ)=fo
      case _=>
    }

  def getCreationFormatValues(forType:Int): Seq[(Int, Constant)] = {
    if (lastFormatableSelection.contains(forType)) {
      val template=lastFormatableSelection(forType)
      val forClass=AllClasses.get.getClassByID(forType)
      for(field<-forClass.formatFields) yield (field,template.getFormatFieldValue(field))
    } else Seq.empty
  }


  protected val selGroup=new SelectGroup[Referencable](EMPTY_OWNERREF,Seq[Referencable]())

    lazy val supportsTouch: Boolean = org.scalajs.dom.window.hasOwnProperty("ontouchstart")

  def setFocusedElement(owner: FocusOwner): Unit = if (focusedElement.isEmpty || focusedElement.get != owner) {
    println("setFocusedElement "+owner+" focusedElem:"+focusedElement)
    for(f<-focusedElement)f.blur()
    focusedElement=Some(owner)
  }

  def select(selection: Iterable[SelectGroup[_ <: Referencable]]): Unit = {
    cellEditor.finishEdit(0, focusTable = false)
    resetOldSelection()
    currentSelection.clear
    for(ngroup<-selection) {
      val list=  getOrCreateChildList(ngroup.parent)
      list++= ngroup.children
    }
    decorateNewSelection()
    storeLastFormatables()
    updateCommonClassID()
  }

  protected def updateCommonClassID():Unit= {
    val commonClassID=AllClasses.get.getCommonClassForGroups(currentSelection)
    commonClass= if(commonClassID>0 ) Some(AllClasses.get.getClassByID(commonClassID)) else None
    notifySelectionChanged()
  }


  def addSelection(selection: Iterable[SelectGroup[_ <: Referencable]],toggle:Boolean): Unit = {
    println("Add Selection "+selection)
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
    storeLastFormatables()
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
    for (cgroup <- currentSelection; elem <- cgroup.children) elem match {
      case dec: SelectionDecorable => dec.hideSelection()
      case _ =>
    }

  def decorateNewSelection(): Unit =
    for (cgroup <- currentSelection; elem <- cgroup.children) elem match {
      case dec: SelectionDecorable => dec.showSelection()
      case _ =>
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
          c.getDescriptionOrName + (if (numElems == 1) " -Objekt" else " -Objekte"/*+"<br>"+c.fieldEditors.mkString(", ")*/), c.actions)
        SidepanelController.loadFieldEditors(c,currentSelection)
      case None =>
        SidepanelController.setSelection(" - ",mutable.LinkedHashMap.empty)
        SidepanelController.clearFieldEditors()
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
        storeFormatable(child)
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

  def containerFocused(container:FocusContainer, propField:Int):Boolean =
    if( lastContainer.isEmpty ||  !(container==lastContainer.get&&container.getOwnerRef==lastOwnerRef&&propField==lastPropField)) {
      println("ContainerFocus "+container.getOwnerRef+" propField"+propField)

      if(lastContainer.isDefined&& container!=lastContainer.get)lastContainer.get.lostSelection()
      if (DialogManager.dialogIsActive) DialogManager.reset()
      shutDown()
      container.getOwnerRef match {
        case Some(contRef) =>
          val theClass = AllClasses.get.getClassByID(contRef.ref.typ).asInstanceOf[WebObjectClass]
          if (theClass.propFields.size > propField) {
            createChildDefs=theClass.propFields(propField).createChildDefs
          } else util.Log.e("wrong propField " + propField + " for class " + theClass)
          selGroup.children = List(contRef)
        //println("set selgroup.children "+selGroup.children)
        case None =>
      }
      lastContainer = Some(container)
      lastPropField = propField
      lastOwnerRef = container.getOwnerRef
      true
    }
  else false

  def focusLastContainer():Unit= for(cl<-lastContainer) cl.requestFocus()

  def shutDown(): Unit ={

  }


}
