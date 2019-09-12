package clientbase.control

import clientbase.connection.WebSocketConnector
import definition.expression.Constant
import definition.typ._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Peter Holzer on 26.12.2015.
  */
object DialogManager {

  var currentAction:Option[ActionTrait]=None
  var currentQuestion:Option[ParamQuestion]=None
  var repeatQuestion:Option[DialogQuestion]=None
  //var currentSelection:Seq[SelectGroup[_ <: Referencable]]=Seq.empty
  var isQuestionRepeating=false

  def dialogIsActive: Boolean = currentQuestion.isDefined

  val answerController=new AnswerController()
  val answerList: ArrayBuffer[(String, Constant)] = scala.collection.mutable.ArrayBuffer[(String, Constant)]()

  def loadAction(newAction:ActionTrait):Unit={
    if(currentAction.isDefined)reset()
    if(SelectionController.currentSelection.nonEmpty) {
      currentAction=Some(newAction)
      newAction.question match {
        case Some(question) => loadQuestion(question)
        case None => for(sg<-SelectionController.currentSelection) {
          WebSocketConnector.executeAction(sg.parent,sg.children,newAction.name,Seq.empty)
          reset()
        }
      }
    }
  }

  def loadQuestion(question: ParamQuestion): Unit = {
    currentQuestion=Some(question)
    question match {
      case d: DialogQuestion =>
        SidepanelController.showAnswerPanel()
        answerController.loadAnswerDefinitions(d)
      case o=> println("Unknown question "+o)
    }
  }

  def reset(): Unit = {
    println("reset currentAction:"+currentAction+"\ncurrentSelection:"+SelectionController.currentSelection)
    for (_ <- currentAction) {
      for(cl<-CreateActionList.lastContainer) cl.actionStopped()
      currentAction=None
      //SelectionController.select(currentSelection)
      currentQuestion=None
      isQuestionRepeating=false
      answerList.clear()
      answerController.reset()
      FieldEditorPanel.showFieldEditors()
      SidepanelController.showActionArea()
    }

  }

  def answerGiven(adef:AnswerDefinition,result:Constant): Unit = if(currentAction.isDefined){
    println("Answer given "+adef.name+" = "+result+" "+result.getType)
    answerList+= ((adef.name,result))
    adef.followQuestion match {
      case Some(fq)=> loadQuestion(fq)
      case None => repeatQuestion match {
        case Some(_) =>
          isQuestionRepeating=true
          loadQuestion(repeatQuestion.get) // if repeatquestion was changed by a custom listener
        case None=>  // action is done
          processResults()
      }
    }
  } else util.Log.e("Answer given without active Action "+adef+" "+result)

  def processResults(): Unit = {
    currentAction match {
      case Some(action)=>
        for(group <-SelectionController.currentSelection)
          WebSocketConnector.executeAction(group.parent,group.children,action.name,answerList)
      case None => util.Log.e("process result without active Action")
    }
    reset()

  }
}
