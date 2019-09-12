package clientbase.control

import clientbase.connection.WebObjectClass
import definition.data.{EMPTY_OWNERREF, Referencable, Reference}
import definition.typ.{AbstractCCD, AllClasses, SelectGroup}

object CreateActionList {
  var lastContainer:Option[FocusContainer]=None
  var lastOwnertRef:Option[Reference]=None
  var lastPropField:Int= -1
  var createChildDefs:Seq[AbstractCCD]=Seq.empty
  private val selGroup=new SelectGroup[Referencable](EMPTY_OWNERREF,Seq[Referencable]())

  def containerFocused(container:FocusContainer, propField:Int):Unit = {
    val cont=Some(container)
    val newContRef=container.getOwnerRef.map(_.ref)
    if(!(cont==lastContainer&&newContRef==lastOwnertRef&&propField==lastPropField)) {
      println("ContainerFocus "+container.getOwnerRef+" propField"+propField)
      //println("container Focused newCont:"+container+" last:"+lastContainer+"\nnewContRef:"+newContRef+" last:"+lastSuperInstRef+"\npropField:"+propField+" last:"+lastPropField)
      if(cont!=lastContainer&lastContainer.isDefined)lastContainer.get.lostSelection()
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
      lastContainer = cont
      lastPropField = propField
      lastOwnertRef = newContRef
    }
  }

  def focusLastContainer():Unit= for(cl<-lastContainer) cl.requestFocus()

  def shutDown(): Unit ={

  }
}
