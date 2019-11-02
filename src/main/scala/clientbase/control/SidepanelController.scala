package clientbase.control

import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.{ColorFieldEditor, LineFormatEditor}
import definition.data.Referencable
import definition.typ.{AbstractObjectClass, ActionTrait, SelectGroup}
import org.scalajs.dom.html.{Button, Input, Label, Paragraph, Span}
//import org.scalajs.dom.html._
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import scalatags.JsDom.all._
import org.scalajs.dom.html.Div


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  selCheck.onclick=(_:MouseEvent)=>{multiSelectMode=selCheck.checked}
  val selectionHeader: Div = div(id:="selheader",`class`:="sel-panel")(label(`class` := "label", float := "left")("Auswahl:  "), selCheck, label(`class` := "label", `for` := "multiSelectCheck", float := "right")("mehrfach")).render
  val selectionLabel: Span = span(`class` := "widelabel", backgroundColor := "#f6f6f6")(" - ").render
  val selectionArea: Div =div(id:="selarea",`class`:="sidepanelpart")(selectionHeader,selectionLabel).render
  val actionLabel: Label = label(`class` := "widelabel")("Aktionen:").render
  protected val actionArea: Div =div(id:="actionArea")(`class`:="sidepanelpart",id:="action")(actionLabel).render

  var currentActions:mutable.LinkedHashMap[String,ActionTrait]=mutable.LinkedHashMap.empty
  val messageLabel: Paragraph =p(`class`:="label")("-").render
  var multiSelectMode=false

  val panelCompTag="panel-comp"
  val fieldEditorsDiv:Div=div(`class`:="fieldeditor-panel").render
  val fieldEditorMap: Predef.Map[String, FieldEditor] = Map("client.graphicsView.ColorFieldEditor"->new ColorFieldEditor,
    "client.graphicsView.LineStyleEditor"-> new LineFormatEditor)


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
    //println("switch onclick "+e)
    if(open) {
      doClose()
      if (sidePanelRoot.contains(selectionArea)) sidePanelRoot.removeChild(selectionArea)
      if (sidePanelRoot.contains(actionArea)) sidePanelRoot.removeChild(actionArea)
      if (sidePanelRoot.contains(fieldEditorsDiv))sidePanelRoot.removeChild(fieldEditorsDiv)
      if (sidePanelRoot.contains(DialogManager.answerController.panel)) sidePanelRoot.removeChild(DialogManager.answerController.panel)
    }
    else {
      doOpen()
      sidePanelRoot.appendChild(selectionArea)
      sidePanelRoot.appendChild(fieldEditorsDiv)
      showActionArea()
    }
    open= !open
  }}

  def setSelection(text: String, actions: mutable.LinkedHashMap[String, ActionTrait]): Unit = {
    println("set selection "+text+" actions:"+actions.size)
    if (sidePanelRoot.contains(DialogManager.answerController.panel)) sidePanelRoot.removeChild(DialogManager.answerController.panel)

    if (open && !sidePanelRoot.contains(actionArea)) sidePanelRoot.appendChild(actionArea)
    //sidePanelRoot.appendChild(FieldEditorPanel.mainDiv)
    currentActions=actions
    selectionLabel.innerHTML=text
    while (actionArea.childElementCount>1)
      actionArea.removeChild(actionArea.lastChild)

    for(action<-actions.values.toIndexedSeq.sortBy(_.buttonID)) {
      val b=button(`class`:="actionbutton")(action.name)(onclick:={()=>{println("action "+action.name);DialogManager.startAction(action)}}).render
      actionArea.appendChild(b)
    }
    actionArea.appendChild(messageLabel)
  }

  def printMessage(st: String): Unit = messageLabel.innerHTML = st

  def addMessage(st: String): Unit = messageLabel.innerHTML = messageLabel.innerHTML + "<br>" + st

  def showAnswerPanel(): Unit = {
    println("showAnswerPanel")
    doOpen()
    hideFieldEditors()
    if (sidePanelRoot.contains(actionArea)) sidePanelRoot.removeChild(actionArea)
    sidePanelRoot.appendChild(DialogManager.answerController.panel)
  }

  def showActionArea():Unit=sidePanelRoot.appendChild(actionArea)

    def panelPart(lname:String,partComp:HTMLElement):Div={
    val elem=div(`class`:="panelpart")(div(`class`:="panel-label")(lname),partComp).render
    partComp.classList.add(panelCompTag)
    elem
  }

  def loadFieldEditors(theClass:AbstractObjectClass, groupList:Seq[SelectGroup[_ <: Referencable]]): Unit = {
    //println("Show Field Editors "+theClass.name)
    clearFieldEditors()
    for (ed<-theClass.fieldEditors;if fieldEditorMap.contains(ed)) {
      val editor=fieldEditorMap(ed)
      fieldEditorsDiv.appendChild(editor.getPanel)
      editor.setData(groupList)
      //println("add "+ed)
    }
  }

  def clearFieldEditors():Unit = {
    while(fieldEditorsDiv.childElementCount>0)
      fieldEditorsDiv.removeChild(fieldEditorsDiv.firstChild)
  }

  def hideFieldEditors():Unit=fieldEditorsDiv.style.visibility="hidden"

  def showFieldEditors():Unit=fieldEditorsDiv.style.visibility="visible"

}
