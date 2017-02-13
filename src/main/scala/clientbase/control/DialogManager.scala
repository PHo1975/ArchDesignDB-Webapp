package clientbase.control

import clientbase.connection.WebSocketConnector
import definition.data.Referencable
import definition.expression.Constant
import definition.typ._

/**
  * Created by Peter Holzer on 26.12.2015.
  */
object DialogManager {

  var currentAction:Option[ActionTrait]=None
  var currentQuestion:Option[ParamQuestion]=None
  var repeatQuestion:Option[DialogQuestion]=None
  var currentSelection:Iterable[SelectGroup[_ <: Referencable]]=Seq.empty
  var isQuestionRepeating=false

  val answerController=new AnswerController()
  val answerList=scala.collection.mutable.ArrayBuffer[(String,Constant)]()

  def loadAction(newAction:ActionTrait):Unit={
    if(currentAction.isDefined)reset()
    currentSelection=SelectionController.currentSelection
    if(currentSelection.nonEmpty) {
      currentAction=Some(newAction)
      newAction.question match {
        case Some(question) => loadQuestion(question)

        case None => for(sg<-currentSelection) {
          WebSocketConnector.executeAction(sg.parent,sg.children,newAction.name,Seq.empty)
          reset()
        }
      }
    }
  }


  def loadQuestion (question:ParamQuestion)= {
    currentQuestion=Some(question)
    question match {
      case d: DialogQuestion => {
        SidepanelController.showAnswerPanel()
        answerController.loadAnswerDefinitions(d)
      }
      case o=> println("Unknown question "+o)
    }
  }

  def reset()={
    for(c<-currentAction){
      currentAction=None
      SelectionController.select(currentSelection)
      currentQuestion=None
      isQuestionRepeating=false
      answerList.clear()
    }

  }

  def answerGiven(adef:AnswerDefinition,result:Constant)= if(currentAction.isDefined){
    println("Answer given "+adef.name+" = "+result+" "+result.getType)
    answerList+= ((adef.name,result))
    adef.followQuestion match {
      case Some(fq)=> loadQuestion(fq)
      case None => repeatQuestion match {
        case Some(rQuestion) =>
          isQuestionRepeating=true
          loadQuestion(repeatQuestion.get) // if repeatquestion was changed by a custom listener
        case None=>  // action is done
          processResults()
      }
    }
  } else util.Log.e("Answer given without active Action "+adef+" "+result)

  def processResults() = {
    currentAction match {
      case Some(action)=>
        for(group <-SelectionController.currentSelection)
          WebSocketConnector.executeAction(group.parent,group.children,action.name,answerList)
      case None => util.Log.e("process result without active Action")
    }
    reset()

  }
}
