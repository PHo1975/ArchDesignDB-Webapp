package clientbase.viewer2d

import building.PartArea
import clientbase.control.{FieldEditor, SidePanelComponent, SidepanelController}
import definition.expression.{Constant, IntConstant}
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.Event
import scalatags.JsDom.all._

class ColorFieldEditor extends FieldEditor {

  val inputField: SidePanelColorInputField =new SidePanelColorInputField(Map(("Plane",3),("PartArea",3),("PolyElem",0),("LineElem",0),("ArcElem",0),
    ("EllipseElem", 0), ("TextElem", 0), ("DimLineElem", 0), ("AreaPolygon", 1), ("BitmapElem", 0), ("PolyLineElem", 0), ("MeasurePolyLine", 1), ("Wohnfläche", 1)),this) {

    addSearchLookup({
      case c:GraphElem => c.color
      case p:PartArea=> p.aufbau
    })
  }

  val allowedClassNames: Iterable[String] = inputField.allowedFields.keys
  val fieldComponents: Seq[SidePanelColorInputField] =Seq(inputField)
  val panel: Div =SidepanelController.panelPart("Farbe",inputField.elem)
  def getPanel: Div =panel

}

trait ColorInputField {
  val elem: Input =input(`type`:="Color").render
  elem.onchange= (e:Event)=>{
    fieldChanged(elem.value)
  }

  def text_=(text:String): Unit ={
    elem.value=text
  }
  def text:String=elem.value
  def fieldChanged(newVal:String):Unit
}

class SidePanelColorInputField(val allowedFields:Map[String,Byte],editor:FieldEditor) extends ColorInputField with SidePanelComponent[Int] {
  def defaultValue=0
  def getConstant(value:Int):Constant=IntConstant(value)
  def valueFromConstant(c: Constant): Int = c.toInt
  def filter(value:Int)=true


  override def setValue(newValue: Option[Int]): Unit = {
    super.setValue(newValue)
    newValue match {
      case Some(dVal)=>
        val tx=((dVal+16777216) & 0xFFFFFF).toHexString
        text="#"+ (if(tx.length<6)tx.reverse.padTo(6,'0').reverse else tx)
      case _ => text=""
    }
  }

  def fieldChanged(newVal: String): Unit = {
    editor.storeValue(Integer.valueOf(newVal.drop(1),16).toInt,this)
  }

}
