package clientbase.control

import definition.expression.ObjectReference
import definition.typ.AnswerDefinition
import org.scalajs.dom.raw.Node

trait ObjectSelectListener {
  def objectSelected(obj: ObjectReference, editable: Boolean): Unit
}



class ObjectAnswerPanel extends AnswerPanel with ObjectSelectListener {
  override def reset(): Unit = {
    for(sel<-SelectionController.currentObjectSelector)
      sel.cancelObjectSelection()
  }

  override def loadAnswerParam(answerDesc: AnswerDefinition): Node ={
    for(sel<-SelectionController.currentObjectSelector)
      sel.askForObjectSelection(answerDesc.constraint,this)
    super.loadAnswerParam(answerDesc)
  }

  override def focus(): Unit = {}

  override def objectSelected(obj: ObjectReference, editable: Boolean): Unit = {
    DialogManager.answerGiven(answerDefinition,obj)
  }
}
