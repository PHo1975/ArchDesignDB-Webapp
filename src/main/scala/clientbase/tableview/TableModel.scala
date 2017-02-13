package clientbase.tableview


import clientbase.connection.{TableSettings, WebSocketConnector}
import clientbase.control._
import definition.data.{EMPTY_OWNERREF, InstanceData, OwnerReference, Reference}
import definition.expression._
import definition.typ.DataType._
import definition.typ.{AllClasses, DataType, FieldSetting, SelectGroup}
import org.scalajs.dom
import org.scalajs.dom.html.{TableCell, TableHeaderCell, TableRow}
import org.scalajs.dom.raw._
import util.{Log, StrToInt}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scalatags.JsDom.all._

/**
 * Created by Peter Holzer on 05.07.2015.
 */
class TableModel(val index:Int,typ:Int,parentNode:Node,pathSubsID:Int,propertyModel:PropertyModel,singleField:Boolean,showClassLabel:Boolean) extends FocusOwner {
  import TableModel._

  var dragIx:Int= -1
  protected val data=new ArrayBuffer[InstanceData]()
  protected val myClass=AllClasses.get.getClassByID(typ)
  protected var tableSettings=TableSettings.getColumnData(typ)
  protected val numCols=if(tableSettings.isEmpty) myClass.fields.size else tableSettings.count(s => myClass.fieldSetting(s.ix).visible)
  protected val myTable=  table(createHeader).render

  var selection:Set[Int]=Set.empty
  val selectionGroup=new SelectGroup[Reference](EMPTY_OWNERREF,Seq.empty)

  val selectionGroupList=List(selectionGroup)
  var createdRow:Option[Int]=None

  var focusedCol:Int=0
  var focusedRow:Int=0
  var shiftDown=false
  var controlDown=false


  myTable.onkeydown={key:KeyboardEvent=>{
    key.keyCode match {
      case 13=> if(!key.getModifierState("Control")) startEditMode()
      case 35=> if(key.getModifierState("Control")) focusCell(focusedCol,numRows) else focusCell(numCols-1,focusedRow);key.preventDefault()// end
      case 36=> if(key.getModifierState("Control")) focusCell(focusedCol,0) else focusCell(0,focusedRow);key.preventDefault() // home
      case 37=> if(focusedCol>0) focusCell(focusedCol-1,focusedRow);key.preventDefault() // left
      case 38=> if(focusedRow>0)focusCell(focusedCol,focusedRow-1);key.preventDefault() // up
      case 39=> if(focusedCol< numCols-1 ) focusCell(focusedCol+1,focusedRow);key.preventDefault() // right
      case 40=> if(focusedRow< numRows-1 ) focusCell(focusedCol,focusedRow+1);key.preventDefault()// down
      case 113=> startEditMode()
      case _=>
    }
  }}

  def numRows: Int =data.size +(if(singleField)0 else 1)

  def createHeader:HTMLTableRowElement=
    tr(th(`class`:="firstcol"),
      for((colData,ix) <-tableSettings.zipWithIndex)
        yield createHeaderCell(myClass.fields(colData.ix).name,ix)).render

  def handleDragStart(event:DragEvent):Unit= {
    event.target match {
      case n:Node =>n.attributes.getNamedItem("ix").value match {
        case StrToInt(ix)=>dragIx=ix
        case o => println("wrong ix "+o)
      }
      case _ =>
    }
    event.dataTransfer.effectAllowed="move"
    event.dataTransfer.dropEffect="move"
    event.dataTransfer.clearData()
    event.dataTransfer.setData("Text","1")
  }

  def createHeaderCell(name:String,ix:Int): TableHeaderCell ={
    val cell=th(draggable:="true",ixAttr:=ix.toString)(name).render

    cell.ondragstart= handleDragStart _
    cell.ondragend=(event:DragEvent)=>{
      cell.style.setProperty("opacity","1")
    }
    cell.ondragover=(event:DragEvent)=> {
      event.preventDefault()
      if(!cell.classList.contains("over"))cell.classList.add("over")
      }

    cell.ondragenter=(event:DragEvent)=> {cell.classList.add("over")}
    cell.ondragleave=(event:DragEvent)=> {cell.classList.remove("over")}
    cell.ondrop=(event:DragEvent)=> {
      event.stopPropagation()
      cell.classList.remove("over")
      if(dragIx > -1) {
        TableSettings.moveColumn(typ,dragIx,ix)
        tableSettings=TableSettings.getColumnData(typ)
        myTable.innerHTML=""
        myTable.appendChild(createHeader)
        var row=0
        for(inst<-data){
          myTable.appendChild(renderRow(inst,row))
          row+=1
        }
      }
      event.preventDefault()
    }
    cell
  }

  setupUI()

  def load(newData:Seq[InstanceData]):Unit = {
    //println("load size:"+newData.size)
    data.clear()
    data ++= newData
    var row=0
    for(inst<-newData){
      myTable.appendChild(renderRow(inst,row))
      row+=1
    }
    if(!singleField)myTable.appendChild(lastRow)
    selection=Set.empty
    focusedRow=0
    focusedCol=0
    createdRow=None
  }

  def removeInstance(ref:Reference):Unit= {
    data.indexWhere(_.ref==ref) match {
      case -1=> Log.e("Remove instance not found "+ref)
      case ix =>
        data.remove(ix)
        myTable.removeChild(myTable.childNodes.item(ix+1))

    }
  }

  def changeInstance(inst:InstanceData):Unit= {
    data.indexWhere(_.ref==inst.ref) match {
      case -1=>
        Log.e("Change instance not found "+inst.ref)
        addInstance(inst)

      case ix =>
        //Log.w("change Inst ref:"+inst.ref+" index:"+ix+" data.length:"+data.size)
        data(ix)=inst
        val renderedRow=renderRow(inst,ix)
        myTable.replaceChild(renderedRow,myTable.childNodes.item(ix+1))
        if(ix==focusedRow&&SelectionController.focusedElement.isDefined&&SelectionController.focusedElement.get==this)
          renderedRow.childNodes.item(focusedCol+1).asInstanceOf[HTMLElement].focus()

    }
  }

  def addInstance(inst:InstanceData): Unit = {
    //println("add instance "+inst.ref)
    data+=inst
    if(myTable.contains(lastRow)) myTable.removeChild(lastRow)
    myTable.appendChild(renderRow(inst,data.size-1))
    myTable.appendChild(lastRow)
  }

  def setupUI():Unit= {
    val theDiv=div().render
    if(showClassLabel) theDiv.appendChild(
      p(`class` := "tablelabel")((
      if (myClass.description.length > 0) myClass.description else myClass.name) + "-Objekte").render )
    theDiv.appendChild(myTable)
    theDiv.appendChild(p(`class`:="tableDist").render)
    parentNode.appendChild(theDiv)
  }

   val lastRow: TableRow =tr(td(`class`:="firstcol"),for(ix<-tableSettings.indices) yield {
      val cell=td(tabindex:="-1")(div(`class`:="inner")(" ")).render
      cell.onclick=(e:MouseEvent)=>if(WebSocketConnector.editable){
        focusedCol=ix
        focusedRow=data.size
        SelectionController.deselect()
        blur()
        if(!SidepanelController.multiSelectMode)
          cell.focus()
      }
      cell.ondblclick={e:MouseEvent => startEditMode();e.stopPropagation();e.preventDefault()}
      cell.oncontextmenu=(e:MouseEvent)=>{
        /*if((e.buttons & 2)>0 ){
          focusedCol=ix
          focusedRow=data.size
          SidepanelController.printMessage("context "+SidepanelController.multiSelectMode)
          if(! SidepanelController.multiSelectMode)startEditMode()
        }*/
        e.stopImmediatePropagation()
        e.preventDefault()
      }
      val longTouch=new LongTouchControl(cell) {
        override def onLongTouch(): Unit = {
          if ( // check if go on
            if (SelectionController.cellEditor.isEditing) {
              if (focusedCol == ix && focusedRow == data.size) false
              else {SelectionController.cellEditor.finishEdit(0); true}
            } else true
          ) {
            SelectionController.deselect()
            blur()
            focusedCol = ix
            focusedRow = data.size
            cell.focus()
            if (!SidepanelController.multiSelectMode) startEditMode()
          }
        }
      }
      longTouch.setup()
      cell
    }
    ).render

  def renderRow(inst:InstanceData,row:Int): TableRow ={
    tr(refAttr:=inst.ref.sToString())(
    td(`class`:="firstcol")(
      button(if(inst.hasChildren)"+" else " ",`class`:="frontbut"),
        onclick:={()=>{println("click "+inst.ref);WebSocketConnector.pathOpenChild(pathSubsID,inst.ref)}}),
    for(ix<- tableSettings.indices;colData =tableSettings(ix);fieldSetting=myClass.fieldSetting(colData.ix))yield
      renderCell(inst,ix,row,colData.ix,fieldSetting)
    ).render
  }

  def renderCell(inst:InstanceData,colIx:Int,row:Int,fieldIx:Int,fieldSetting:FieldSetting): TableCell ={
    val field=myClass.fields(fieldIx)
    val expression=inst.fieldData(fieldIx)
    val st=if(expression== null || expression.isNullConstant) {
      if(field.typ ==DataType.BoolTyp) "\u25cc"
      else "\u00A0"
    }else if(fieldSetting.showFormula) expression.getTerm
    else{
      val value=expression.getValue
      field.typ match {
        case DataType.DoubleTyp | DataType.UnitNumberTyp =>
          try {
            val unitAdd = if (value.getType == DataType.UnitNumberTyp) " " + value.toUnitNumber.unitFraction.toString else ""
            if (fieldSetting.formString.length > 0) fieldSetting.formString.format(value.toDouble) + unitAdd
            else value.toDouble.toString + unitAdd
          } catch {case NonFatal(e)=>Log.e("field:"+(colIx-1)+" formString:"+fieldSetting.formString,e); value.toDouble.toString}
        case DataType.BoolTyp =>
          if(value.toBoolean) "\u221A" else "\u25a1"
        case VectorTyp => value.toVector.shortToString
        case CurrencyTyp =>  f"${value.toDouble}%,.2f "+CurrencyConstant.currencySign
        case _ => value.toString.replace('\n','|')

      }
    }
    val inner=div(`class`:="inner")(st).render
    val cell=td(tabindex:="-1")(inner).render

    def onClicked(control:Boolean, shift:Boolean)=if(WebSocketConnector.editable){
      cell.focus()
      SelectionController.setFocusedElement(this)
      focusCellClicked(colIx,row,control,shift)
    }

    if(field.typ==DataType.DoubleTyp||field.typ==DataType.IntTyp||field.typ==DataType.CurrencyTyp)
      cell.classList.add("numbercell")

    inner.onmousedown=(e:MouseEvent)=>{
      controlDown=e.getModifierState("Control")
      shiftDown=e.getModifierState("Shift")
    }
    cell.oncontextmenu=(e:MouseEvent)=>{
      //SelectionController.sidePanelController.get.printMessage("edit "+e.buttons)
      /*if((e.buttons & 2)>0 ) {
        startEditMode()
      }*/
      e.stopImmediatePropagation()
      e.preventDefault()
    }

    val longTouch=new LongTouchControl(cell) {
      override def onLongTouch(): Unit = {
        if ( // check if go on
          if( SelectionController.cellEditor.isEditing) {
            if(focusedCol==colIx&& focusedRow==row) false
            else { SelectionController.cellEditor.finishEdit(0); true }
          } else true
        ) {
          focusedCol = colIx
          if (SidepanelController.multiSelectMode) {
            //SidepanelController.addMessage("select "+row+" - "+focusedRow)
            selectInterval(focusedRow, row)
            //cell.focus()
            SelectionController.setFocusedElement(TableModel.this)
            focusedRow = row
          } else {
            focusedRow = row
            startEditMode()
          }
        }
      }
    }
    longTouch.setup()
    cell.onfocus=(e:FocusEvent)=>{
      if(!SidepanelController.multiSelectMode || !longTouch.timerUsed)
      onClicked(controlDown,shiftDown)
    }

    inner.ondblclick={e:MouseEvent => startEditMode();e.stopPropagation();e.preventDefault()}
    cell
  }

  def startEditMode():Unit= {
    //println("start edit "+focusedRow)
    if(SelectionController.cellEditor.isEditing)SelectionController.cellEditor.finishEdit(0,false)
    val fieldIx = tableSettings(focusedCol).ix
    var instRef:Option[Reference]=None
    val text=if(focusedRow>data.size-1) "" else {
      val inst = data(focusedRow)
      instRef=Some(inst.ref)
      inst.fieldData(fieldIx) match {
        case StringConstant(st) => st
        case expression => expression.getTerm
      }
    }
    val parent=myTable.childNodes.item(focusedRow+1).childNodes.item(focusedCol+1)
    SelectionController.cellEditor.startEdit(text,parent,(newText,dir)=>{
      if(instRef.isEmpty) {
        WebSocketConnector.createInstance(typ,Array(new OwnerReference(propertyModel.propField,propertyModel.topRef)),const=>{
          writeField(new Reference(typ,const.toInt),fieldIx,newText)
        })
      }
      else for(ref<-instRef) writeField(ref,fieldIx,newText)
    })
  }

  protected def writeField(ref:Reference,fieldIx:Int,newText:String):Unit= {
    StringParser.parse(newText) match {
      case ex:Expression=>WebSocketConnector.writeInstanceField(ref,fieldIx,ex)
      case error:ParserError=> if(myClass.fields(fieldIx).typ==DataType.StringTyp)
        WebSocketConnector.writeInstanceField(ref,fieldIx,new StringConstant(newText))
      else dom.window.alert("Fehler: "+error.message)
    }
    focusCell(focusedCol,focusedRow,notify=false)
  }


  def focusCell(col:Int,row:Int,notify:Boolean=true):Unit=
    if(col< tableSettings.size&& row<numRows){
      myTable.childNodes.item(row+1).childNodes.item(col+1) match {
        case el:HTMLTableCellElement=> el.focus()
        case other=> println(" no html "+other+" "+other.getClass)
      }
      if(notify) {
        focusedCol=col
        focusedRow=row
        SelectionController.cellEditor.finishEdit(0)
      }
      if(singleField||row!=numRows-1) selectInterval(row,row)
    }

  def focusCellClicked(col:Int,row:Int,control:Boolean,shift:Boolean):Unit= {
    //SidepanelController.addMessage("click "+row)
    if(SidepanelController.multiSelectMode) toggleSelection(row)
    else if(control) toggleSelection(row)
    else if (shift) selectInterval(focusedRow,row)
    else selectInterval(row,row)

    focusedCol=col
    focusedRow=row
    SelectionController.cellEditor.finishEdit(0)
  }

  def blur():Unit={
    selection=Set.empty
    clearSelectStyle()
  }

  // selection

  protected def notifySelection():Unit={
    selectionGroup.children= for(s<-selection; if s<data.size) yield data(s).ref
    SelectionController.select(selectionGroupList)
  }

  protected def selectInterval(start:Int, end:Int):Unit= {
    clearSelectStyle()
    val myStart=math.min(start,end)
    val myEnd=math.max(start,end)
    val range=myStart to myEnd
    selection=range.toSet
    //SidepanelController.addMessage("range "+range)
    for(pos <-range) addSelectStyle(pos)
    notifySelection()
  }

  protected def toggleSelection(pos:Int):Unit= {
    if(selection contains pos) {
      selection -=pos
      removeSelectStyle(pos)
    }
    else {
      selection+=pos
      addSelectStyle(pos)
    }
    notifySelection()
  }

  protected def addSelectStyle(row:Int):Unit= {
    val rowData=myTable.childNodes.item(row+1)
    for (col<-0 until numCols)rowData.childNodes.item(col+1).asInstanceOf[HTMLElement].classList.add("selection")
  }

  protected def removeSelectStyle(row:Int):Unit= {
    val rowData=myTable.childNodes.item(row+1)
    for (col<-0 until numCols)rowData.childNodes.item(col+1).asInstanceOf[HTMLElement].classList.remove("selection")
  }

  protected def clearSelectStyle():Unit= {
    for (row<-data.indices;rowData=myTable.childNodes.item(row+1);col<-0 until numCols)
      rowData.childNodes.item(col+1).asInstanceOf[HTMLElement].classList.remove("selection")
  }





  // interface FocusOwner
  def getPrevOwner:Option[FocusOwner]= propertyModel.getPrevTable(index)
  def getNextOwner:Option[FocusOwner]= propertyModel.getNextTable(index)
  def focusFromUp():Unit={
    focusCell(0,0)
    SelectionController.setFocusedElement(this)
  }
  def focusFromBottom():Unit={
    focusCell(0,data.size-1)
    SelectionController.setFocusedElement(this)
  }

}

object TableModel {
  val refAttr: scalatags.JsDom.all.Attr =attr("ref")
  val ixAttr: scalatags.JsDom.all.Attr=attr("ix")
}