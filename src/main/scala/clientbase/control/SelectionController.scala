package clientbase.control

import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.SelectionDecorable
import definition.data.Referencable
import definition.typ.{ AbstractObjectClass, AllClasses, SelectGroup }
import scala.collection.mutable

/**
 * Created by Peter Holzer on 20.09.2015.
 */

object SelectionController {
  var currentSelection:Iterable[SelectGroup[_ <: Referencable]]=Seq.empty

  val cellEditor=new CellEditor

  var focusedElement:Option[FocusOwner]=None

  var commonClass:Option[AbstractObjectClass]=None

  lazy val supportsTouch: Boolean = org.scalajs.dom.window.hasOwnProperty("ontouchstart")

  def setFocusedElement(owner: FocusOwner): Unit = if (focusedElement.isEmpty || focusedElement.get != owner) {
    for(f<-focusedElement)f.blur()
    focusedElement=Some(owner)
  }

  def select(selection: Iterable[SelectGroup[_ <: Referencable]]): Unit = {
    cellEditor.finishEdit(0, focusTable = false)
    resetOldSelection()
    currentSelection=selection
    decorateNewSelection()
    val commonClassID=AllClasses.get.getCommonClassForGroups(selection)
    commonClass= if(commonClassID>0 ) Some(AllClasses.get.getClassByID(commonClassID)) else None
    notifySelectionChanged()
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
    currentSelection=Seq.empty
    commonClass=None
    notifySelectionChanged()
  }

  def notifySelectionChanged(): Unit = if (WebSocketConnector.editable) {
    val numElems=currentSelection.foldLeft(0)((sum,el)=>sum+el.children.size)
    commonClass match {
      case Some(c) => SidepanelController.setSelection("" + numElems + " " +
        c.getDescriptionOrName + (if (numElems == 1) " -Objekt" else " -Objekte"), c.actions)
      case None => SidepanelController.setSelection(" - ",mutable.LinkedHashMap.empty)
    }
  }

  def printMessage(text: String): Unit = SidepanelController.messageLabel.innerHTML = text
}
