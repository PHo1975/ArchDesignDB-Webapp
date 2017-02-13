package clientbase.control

import definition.data.Referencable
import definition.typ.{AbstractObjectClass, SelectGroup, AllClasses}

import scala.collection.mutable

/**
 * Created by Peter Holzer on 20.09.2015.
 */

object SelectionController {
  var currentSelection:Iterable[SelectGroup[_ <: Referencable]]=Seq.empty

  val cellEditor=new CellEditor

  var focusedElement:Option[FocusOwner]=None

  var commonClass:Option[AbstractObjectClass]=None

  lazy val supportsTouch=org.scalajs.dom.window.hasOwnProperty("ontouchstart")

  def setFocusedElement(owner:FocusOwner)=if(focusedElement.isEmpty||focusedElement.get!=owner){
    for(f<-focusedElement)f.blur()
    focusedElement=Some(owner)
  }

  def select(selection:Iterable[SelectGroup[_ <: Referencable]])={
    cellEditor.finishEdit(0,false)
    currentSelection=selection
    val commonClassID=AllClasses.get.getCommonClassForGroups(selection)
    commonClass= if(commonClassID>0 ) Some(AllClasses.get.getClassByID(commonClassID)) else None
    notifySelectionChanged()
  }

  def deselect()= {
    cellEditor.finishEdit(0,false)
    currentSelection=Seq.empty
    commonClass=None
    notifySelectionChanged()
  }

  def notifySelectionChanged()= {
    val numElems=currentSelection.foldLeft(0)((sum,el)=>sum+el.children.size)

    commonClass match {
      case Some(c)=> {
        SidepanelController.setSelection(""+numElems+" "+c.getDescriptionOrName+(if(numElems==1)" -Objekt" else " -Objekte"), c.actions )
      }
      case None => SidepanelController.setSelection(" - ",mutable.LinkedHashMap.empty)
    }

  }

  def printMessage(text:String)= SidepanelController.messageLabel.innerHTML=text
}
