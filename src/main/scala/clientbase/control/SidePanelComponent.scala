package clientbase.control

import definition.data.{InstanceData, Referencable}
import definition.expression._
import definition.typ.DataType
import org.scalajs.dom
import org.scalajs.dom.html.{Input, Select}
import org.scalajs.dom.raw.Event
import scalatags.JsDom.all._

trait SidePanelComponent[A] {
  def allowedFields:Map[String,Byte] // "ClassName" -> DBFieldNr
  var fieldMap:Map[Int,Byte]=Map.empty // ClassID -> DBFieldNr
  var currentValue:Option[A]=None

  var elemToValueLookup:PartialFunction[Referencable,A]={
    case inst:InstanceData if fieldMap.contains(inst.ref.typ) =>
      valueFromConstant(inst.fieldValue(fieldMap(inst.ref.typ)))
  }

  protected var searchValue: Option[A] = _

  /**
  * @param nameMap map aus erlaubten ClassenNamen und zugehörigen Typ-IDs
*/
  def createFieldMap(nameMap: Map[String, Int]): Unit = fieldMap = allowedFields.map(elem => nameMap(elem._1) -> elem._2.toByte)

  def defaultValue:A

  def setValue(newValue:Option[A]):Unit= currentValue=newValue

  def getConstant(value:A):Constant

  def valueFromConstant(c:Constant):A

  def resetSearchValue():Unit= searchValue=null

  def internSetSearchValue(newValue: A): Unit = if (searchValue == null) searchValue = Some(newValue)
  else if(searchValue.isDefined && searchValue.get!=newValue) searchValue=None

  def checkSearchValue(value:Referencable):Unit= {
    if(elemToValueLookup.isDefinedAt(value)) internSetSearchValue(elemToValueLookup(value))
  }

  def addSearchLookup(newCase: PartialFunction[Referencable, A]): Unit = {
    elemToValueLookup=newCase orElse elemToValueLookup
  }

  def updateSearchValue():Unit = {
    if(searchValue==null) searchValue=None
    setValue(searchValue)
    resetSearchValue()
  }
}



trait IntSidePanelComponent extends SidePanelComponent[Int] {
  def defaultValue=0

  def getConstant(value: Int): Constant = IntConstant(value)
  def valueFromConstant(c:Constant):Int=c.toInt
}


trait ActiveInputField {
  val elem: Input =input(`type`:="Text").render
  elem.onchange= (e:Event)=>{
    println("Changed "+elem.value)
    fieldChanged(elem.value)
  }
  def text_=(text:String): Unit ={
    println("set Text:"+text)
    elem.value=text
  }
  def text:String=elem.value
  def fieldChanged(newVal:String):Unit
}

abstract class ActiveComboBox[A] (items:Seq[A]) {
  val elem:Select= select(items.map( item=> option( item.toString).render)).render

  elem.onchange= (e:Event)=>{
    elem.selectedIndex match {
      case -1 =>
      case ix => elemClicked(items(ix))
    }
  }
  def elemClicked(item:A):Unit
}

abstract class SidePanelComboBox[A](items: Seq[A],editor:FieldEditor,val allowedFields:Map[String,Byte])
  extends ActiveComboBox[A](items) with SidePanelComponent[A] {

  def elemClicked(item: A): Unit = editor.storeValue(item, this)
  override def setValue(newValue: Option[A]): Unit = {
    super.setValue(newValue)
    elem.selectedIndex = newValue match {
      case Some(theValue)=>items.indexOf(theValue)
      case None => -1
    }
  }
}




class SidePanelDoubleInputField(val allowedFields:Map[String,Byte],editor:FieldEditor) extends ActiveInputField with SidePanelComponent[Double] {
  def defaultValue=0d
  def getConstant(value:Double):Constant=new DoubleConstant(value)
  def valueFromConstant(c: Constant): Double = c.toDouble
  def filter(value:Double)=true
  def precision:Int=3
  val formatPattern: String = "%3." + precision + "f"

  override def setValue(newValue: Option[Double]): Unit = {
    super.setValue(newValue)
    newValue match {
      case Some(dVal)=> text=formatPattern.format(dVal)
      case _ => text=""
    }
  }

  def fieldChanged(newVal: String): Unit = {
    StringParser.parse(text, DataType.DoubleTyp) match {
      case ParserError(message,pos)=>
        elem.selectionStart=pos
        elem.selectionEnd=pos
        dom.window.alert(message)
      case exp:Expression=>
        editor.storeValue(exp.getValue.toDouble,this)
    }
  }
}


