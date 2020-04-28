package clientbase.control

import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.GraphQuestionHandler
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
  var actionGroups:Iterable[SelectGroup[_<:Referencable]] = Seq.empty

  var isQuestionRepeating=false
  var hasRebound=false
  var createType:Option[Int]=None
  var propField:Byte= -1
  var createdNewElements=0

    val answerController=new AnswerController()
  val answerList: ArrayBuffer[ResultElement] = scala.collection.mutable.ArrayBuffer[ResultElement]()
  var customAnswerListener: List[(Iterable[ResultElement] => Unit, Option[ParamQuestion])] = Nil



  def startAction(newAction:ActionTrait):Unit={
    if(dialogIsActive&& ! hasRebound)reset()
    if(hasRebound) hasRebound=false
    if(SelectionController.currentSelection.nonEmpty) {
      currentAction=Some(newAction)
      newAction.question match {
        case Some(question) =>
          actionGroups=SelectionController.currentSelection
          loadQuestion(question)
        case None => for(sg<-SelectionController.currentSelection) {
          //println("Execute Action without questions parent:"+sg.parent+" children:"+sg.children+" name:"+newAction.name)
          WebSocketConnector.executeAction(sg.parent,sg.children,newAction.name,Seq.empty)
          reset()
        }
      }
    }
  }

  def startCreateAction(action:ActionTrait,ncreateType:Int,npropField:Byte):Unit ={
    //println("StartCreateAction "+action)
    if(dialogIsActive&& ! hasRebound)reset()
    if(hasRebound) hasRebound=false
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

  def startIntermediateQuestion(question:ParamQuestion,listener:(Iterable[ResultElement]) => Unit,storeAnswer:Boolean=true):Unit= {
    customAnswerListener=((listener,if(!storeAnswer)currentQuestion else None)) :: customAnswerListener
    loadQuestion(question)
  }



  protected def createNewElem (actionName:String): Unit = {
    for(lastC<-SelectionController.lastContainer;ownerRef<-lastC.getOwnerRef;ncreateType<-createType) {
      lastC.createActionSubmitted(1)
      val formatValues = SelectionController.getCreationFormatValues(createType.get)
      WebSocketConnector.executeCreateAction(ownerRef.ref,propField,ncreateType,actionName,Seq.empty,formatValues)
    }
  }


  def loadQuestion(question: ParamQuestion): Unit = {
    currentQuestion=Some(question)
    question match {
      case d: DialogQuestion =>
        repeatQuestion=if(d.repeat) Some(d) else None
        SidepanelController.showAnswerPanel()
        answerController.loadAnswerDefinitions(d)
      case c:CommandQuestion=>
        repeatQuestion=None
        for(cont<-SelectionController.lastContainer)
          getCustomQuestionHandler(c.module).load(c,cont)
      case o=> println("Unknown question "+o)
    }
  }

  def dialogIsActive: Boolean =currentQuestion.isDefined

  def reset(): Unit = {
    println("reset currentAction:"+currentAction+"\ncurrentSelection:"+SelectionController.currentSelection+" is repeating:"+isQuestionRepeating+" answerlist:"+answerList)
    if(dialogIsActive) {
      if(isQuestionRepeating&&answerList.nonEmpty){
        isQuestionRepeating=false
        processResults()
      }
      for(cl<-SelectionController.lastContainer) cl.actionStopped()
      currentAction=None
      //SelectionController.select(currentSelection)
      for(c<-currentQuestion) c match {
        case pq:CustomPanelQuestion=> pq.panel.shutDown()
        case _=>
      }
      currentQuestion=None
      actionGroups=Seq.empty
      isQuestionRepeating=false
      repeatQuestion=None
      answerList.clear()
      answerController.reset()
      customAnswerListener=Nil
      createType=None
      SidepanelController.removeAnswerPanel()
      SidepanelController.showFieldEditors()
      SidepanelController.showActionArea()
    }

  }

  def answerGiven(adef:AnswerDefinition,result:Constant): Unit = if(dialogIsActive && !hasRebound){
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
          println("Listener "+listener+" lastQuestion:"+lastQuestion+" numListener"+customAnswerListener.size)
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
                case None =>
                  //val answerBackup=answerList.clone()
                  listener(answerList)
              }
          }
        }
    }
  } else util.Log.e("Answer given without active Action "+adef+" "+result)



  def processResults(): Unit = {
    var repeatWithoutCAS=false
    println("ProcessResults actiongroups "+actionGroups+" createType:"+ createType)
    if(actionGroups.nonEmpty){
      currentAction match {
        case Some(ca) if(ca.question.isDefined&&ca.question.get.repeat)||ca.rebound=>
          hasRebound=true
          SelectionController.lastContainer match{
            case Some(lc) if lc.hasCreateActionStarted=>
              lc.onCreatedDataReceived(()=>repeatAction(ca,createType,propField))
            case _=> repeatWithoutCAS=true
          }
        case _ => hasRebound=false
      }
    }
    createType match {
      case Some(ctype)=>
         SelectionController.lastContainer match {
          case Some(lc) =>
            //println("container:"+lc+" currentAction:"+currentAction+" owner:"+lc.getOwnerRef.get.ref)
            lc.createActionSubmitted(if(createdNewElements==0) 1 else createdNewElements)
            val formatValues: Seq[(Int, Constant)] =SelectionController.getCreationFormatValues(ctype)
            for(cAction<-currentAction;owner<-lc.getOwnerRef) {
              //println("Execute "+owner.ref+" pr:"+propField+" ctype:"+ctype+" Action:"+cAction.name+" Answers:"+answerList+" format:"+formatValues)
              WebSocketConnector.executeCreateAction(owner.ref,propField,ctype,cAction.name,answerList,formatValues)
            }

          case None=> Log.e("Process results, create Type "+ctype+" but no last Container")

        }
      case None => for(action<-currentAction;group <-actionGroups)
        WebSocketConnector.executeAction(group.parent,group.children,action.name,answerList)
    }
    if(!hasRebound)reset()
    if(repeatWithoutCAS)
      for(ca<-currentAction) repeatAction(ca,createType,propField)
  }

  protected def repeatAction(ca: ActionTrait, crType: Option[Int], prField: Byte): Unit =
    //println("Repeat "+createType+" "+ca.name+" "+ actionGroups+" dialogIsActive:"+dialogIsActive+" "+Thread.currentThread().getName)
    crType match {
      case Some(ct)=>startCreateAction(ca,ct,prField)
      case None =>  startAction(ca)
    }



  def increaseNumCreatedElements(amount: Int = 1): Unit = createdNewElements += amount

  def getCustomQuestionHandler(moduleType: ModuleType.Value):CustomQuestionHandler= moduleType match {
    case ModuleType.Graph=>GraphQuestionHandler
    case o=> throw new IllegalArgumentException("Unknown module "+o)
  }
}
