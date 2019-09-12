package clientbase.control

import clientbase.viewer2d.{ColorFieldEditor, LineFormatEditor}
import definition.data.Referencable
import definition.typ.{AbstractObjectClass, SelectGroup}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._

object FieldEditorPanel {
  val panelCompTag="panel-comp"
  val mainDiv:Div=div(`class`:="fieldeditor-panel").render


  def panelPart(lname:String,partComp:HTMLElement):Div={
    val elem=div(`class`:="panelpart")(div(`class`:="panel-label")(lname),partComp).render
    partComp.classList.add(panelCompTag)
    elem
  }

  val fieldEditorMap: Map[String, FieldEditor] =Map("client.graphicsView.ColorFieldEditor"->new ColorFieldEditor,
    "client.graphicsView.LineStyleEditor"-> new LineFormatEditor)

  def loadFieldEditors(theClass:AbstractObjectClass, groupList:Seq[SelectGroup[_ <: Referencable]]): Unit = {
    //println("Show Field Editors "+theClass.name)
    clearFieldEditors()
    for (ed<-theClass.fieldEditors;if fieldEditorMap.contains(ed)) {
      val editor=fieldEditorMap(ed)
      mainDiv.appendChild(editor.getPanel)
      editor.setData(groupList)
      //println("add "+ed)
    }
  }

  def clearFieldEditors():Unit = {
    while(mainDiv.childElementCount>0)
      mainDiv.removeChild(mainDiv.firstChild)
  }

  def hideFieldEditors():Unit=mainDiv.style.visibility="hidden"

  def showFieldEditors():Unit=mainDiv.style.visibility="visible"

}
