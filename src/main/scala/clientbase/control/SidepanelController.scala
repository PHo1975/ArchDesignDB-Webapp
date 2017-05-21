package clientbase.control

import clientbase.connection.WebSocketConnector
import definition.typ.ActionTrait
import org.scalajs.dom.html._
import org.scalajs.dom.raw.{ HTMLElement, MouseEvent }
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 12.12.2015.
  */
object SidepanelController {
  var sidePanelRoot:HTMLElement=_
  var contentRoot:HTMLElement=_
  val openTag="sidepanelopen"
  val closeTag="sidepanelclosed"
  val openCTag="contentopen"
  val closeCTag="contentclosed"

  val switchListener: ArrayBuffer[() ⇒ Unit] = ArrayBuffer[() => Unit]()

  val switchButton: Button =button(id:="switchbut",tabindex:= -1)("\u2261").render
  var open=false
  val selCheck: Input = input(`type` := "checkbox", id := "multiSelectCheck", float := "right").render
  selCheck.onclick=(e:MouseEvent)=>{multiSelectMode=selCheck.checked}
  val selectionHeader: Div = div(label(`class` := "label", float := "left")("Auswahl:  "), selCheck, label(`class` := "label", `for` := "multiSelectCheck", float := "right")("mehrfach")).render
  val selectionLabel: Span = span(`class` := "label", width := "100%", backgroundColor := "#f6f6f6", float := "left")(" - ").render
  val selectionArea: Div =div(`class`:="sidepanelpart")(selectionHeader,selectionLabel).render
  val actionLabel: Label = label(`class` := "label", float := "left")("Aktionen:").render
  val actionArea: Div =div(`class`:="sidepanelpart",id:="action")(actionLabel).render

  var currentActions:mutable.LinkedHashMap[String,ActionTrait]=mutable.LinkedHashMap.empty
  val messageLabel: Paragraph =p(`class`:="label")("-").render
  var multiSelectMode=false

  def setup(nsidePanelRoot: HTMLElement, ncontentRoot: HTMLElement): Unit = {
    sidePanelRoot=nsidePanelRoot
    contentRoot=ncontentRoot
    if (WebSocketConnector.editable) sidePanelRoot.appendChild(switchButton)
    else {
      sidePanelRoot.style.width = "0"
      contentRoot.style.marginLeft = "0"
      contentRoot.style.width = "100%"
    }

  }

  def notifySwitchListeners(): Unit = for (s <- switchListener) s()

  def doOpen(): Unit = if (WebSocketConnector.editable) {
    sidePanelRoot.classList.remove(closeTag)
    contentRoot.classList.remove(closeCTag)
    sidePanelRoot.classList.add(openTag)
    contentRoot.classList.add(openCTag)
    switchButton.innerHTML="<"
    notifySwitchListeners()
  }

  def doClose(): Unit = if (WebSocketConnector.editable) {
    sidePanelRoot.classList.remove(openTag)
    contentRoot.classList.remove(openCTag)
    sidePanelRoot.classList.add(closeTag)
    contentRoot.classList.add(closeCTag)
    switchButton.innerHTML="\u2261"
    notifySwitchListeners()
  }


  switchButton.onclick= {e:MouseEvent=> {
    println("switch onclick "+e)
    if(open) {
      doClose()
      if (sidePanelRoot.contains(selectionArea)) sidePanelRoot.removeChild(selectionArea)
      if (sidePanelRoot.contains(actionArea)) sidePanelRoot.removeChild(actionArea)
      if (sidePanelRoot.contains(DialogManager.answerController.panel)) sidePanelRoot.removeChild(DialogManager.answerController.panel)
    }
    else {
      doOpen()
      sidePanelRoot.appendChild(selectionArea)
      sidePanelRoot.appendChild(actionArea)
    }
    open= !open
  }}

  def setSelection(text: String, actions: mutable.LinkedHashMap[String, ActionTrait]): Unit = {
    //println("set selection "+text+" actions:"+actions.size)
    if (sidePanelRoot.contains(DialogManager.answerController.panel)) sidePanelRoot.removeChild(DialogManager.answerController.panel)
    if (open && !sidePanelRoot.contains(actionArea)) sidePanelRoot.appendChild(actionArea)
    currentActions=actions
    selectionLabel.innerHTML=text
    while (actionArea.childElementCount>1)
      actionArea.removeChild(actionArea.lastChild)

    for(action<-actions.values.toIndexedSeq.sortBy(_.buttonID)) {
      val b=button(`class`:="actionbutton")(action.name)(onclick:={()=>{println("action "+action.name);DialogManager.loadAction(action)}}).render
      actionArea.appendChild(b)
    }
    actionArea.appendChild(messageLabel)
  }

  def printMessage(st: String): Unit = messageLabel.innerHTML = st

  def addMessage(st: String): Unit = messageLabel.innerHTML = messageLabel.innerHTML + "<br>" + st

  def showAnswerPanel(): Unit = {
    doOpen()
    if (sidePanelRoot.contains(actionArea)) sidePanelRoot.removeChild(actionArea)
    sidePanelRoot.appendChild(DialogManager.answerController.panel)
  }
}
