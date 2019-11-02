package clientbase.control

import clientbase.connection.WebSocketConnector
import definition.data.{Referencable, ResultElement}
import definition.expression.Constant
import definition.typ._
import util.Log

import scala.collection.mutable.ArrayBuffer

case class CustomPanelQuestion(override val panel:CustomPanel) extends PanelQuestion{
  def toXML:scala.xml.Node=null

  def name: String = panel.name
  def classID=5
}

/**
  * Created by Peter Holzer on 26.12.2015.
  */
object DialogManager {

  var currentAction:Option[ActionTrait]=None
  var currentQuestion:Option[ParamQuestion]=None
  var repeatQuestion:Option[DialogQuestion]=None
  var actionGroups:Iterable[SelectGroup[_<:Referencable]] = _

  var isQuestionRepeating=false
  var createType:Option[Int]=None
  var propField:Byte= -1
  var createdNewElements=0

  def dialogIsActive: Boolean = currentQuestion.isDefined

  val answerController=new AnswerController()
  val answerList: ArrayBuffer[ResultElement] = scala.collection.mutable.ArrayBuffer[ResultElement]()
  var customAnswerListener: List[(Seq[ResultElement] => Unit, Option[ParamQuestion])] = Nil



  def startAction(newAction:ActionTrait):Unit={
    if(currentAction.isDefined)reset()
    if(SelectionController.currentSelection.nonEmpty) {
      currentAction=Some(newAction)
      newAction.question match {
        case Some(question) =>
          actionGroups=SelectionController.currentSelection
          loadQuestion(question)
        case None => for(sg<-SelectionController.currentSelection) {
          WebSocketConnector.executeAction(sg.parent,sg.children,newAction.name,Seq.empty)
          reset()
        }
      }
    }
  }

  def startCreateAction(action:ActionTrait,ncreateType:Int,npropField:Byte):Unit ={
    if(currentAction.isDefined)reset()
    action.question match {
      case Some(question)=>
        createdNewElements=0
        createType=Some(ncreateType)
        propField=npropField
        currentAction=Some(action)
        loadQuestion(question)
      case None =>
        createNewElem(action.name)
    }
  }

  def startIntermediateQuestion(question:ParamQuestion,listener:(Seq[ResultElement]) => Unit,storeAnswer:Boolean=true):Unit= {
    customAnswerListener=(listener,if(!storeAnswer)currentQuestion else None) :: customAnswerListener
    loadQuestion(question)
  }



  protected def createNewElem (actionName:String): Unit = {
    for(lastC<-SelectionController.lastContainer;ownerRef<-lastC.getOwnerRef;ncreateType<-createType) {
      lastC.createActionSubmitted(1)
      val formatValues = lastC.getCreationFormatValues(createType.get)
      WebSocketConnector.executeCreateAction(ownerRef.ref,propField,ncreateType,actionName,Seq.empty,formatValues)
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
      for(cl<-SelectionController.lastContainer) cl.actionStopped()
      currentAction=None
      //SelectionController.select(currentSelection)
      for(c<-currentQuestion) c match {
        case pq:CustomPanelQuestion=> pq.panel.shutDown()
        case _=>
      }
      currentQuestion=None
      isQuestionRepeating=false
      answerList.clear()
      answerController.reset()
      customAnswerListener=Nil
      createType=None
      SidepanelController.showFieldEditors()
      SidepanelController.showActionArea()
    }

  }

  def answerGiven(adef:AnswerDefinition,result:Constant): Unit = if(currentAction.isDefined){
    println("Answer given "+adef.name+" = "+result+" "+result.getType)
    val answer=ResultElement(adef.name,result)
    answerList+= answer
    adef.followQuestion match {
      case Some(fq)=> loadQuestion(fq)
      case None =>
        if(customAnswerListener.isEmpty) {
          repeatQuestion match {
            case Some(question) =>
              isQuestionRepeating = true
              loadQuestion(question) // if repeatquestion was changed by a custom listener
            case None => // action is done
              processResults()
          }
        } else {
          val (listener,lastQuestion)=customAnswerListener.head
          //println("Listener "+listener+" lastQuestion:"+lastQuestion)
          repeatQuestion match {
            case Some(rQuestion) =>
              isQuestionRepeating = true
              rQuestion match {
                case dq: DialogQuestion => answerController.loadAnswerDefinitions(dq) // update pointpanel precision
                case _ =>
              }
              listener(answerList)
            case None=>
              customAnswerListener=customAnswerListener.tail
              lastQuestion match {
                case Some(lquestion)=>
                  answerList.remove(answerList.size-1)
                  listener(Seq(answer))
                  loadQuestion(lquestion)
                case None => listener(answerList)//;processResults()
              }
          }
        }
    }
  } else util.Log.e("Answer given without active Action "+adef+" "+result)



  def processResults(): Unit = {
    createType match {
      case Some(ct)=>
         SelectionController.lastContainer match {
          case Some(lc) =>
            lc.createActionSubmitted(if(createdNewElements==0) 1 else createdNewElements)
            val formatValues=lc.getCreationFormatValues(ct)
            for(cAction<-currentAction;owner<-lc.getOwnerRef)
              WebSocketConnector.executeCreateAction(owner.ref,propField,ct,cAction.name,answerList,formatValues)

          case None=> Log.e("Process results, create Type "+ct+" but no last Container")

        }
      case None => for(action<-currentAction;group <-actionGroups)
        WebSocketConnector.executeAction(group.parent,group.children,action.name,answerList)
    }
    reset()
  }


  def increaseNumCreatedElements(amount: Int = 1): Unit = createdNewElements += amount
}
