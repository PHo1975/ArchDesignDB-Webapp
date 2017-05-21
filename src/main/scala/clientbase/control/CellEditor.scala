package clientbase.control

import org.scalajs.dom.document
import org.scalajs.dom.html.TextArea
import org.scalajs.dom.raw.{ Event, HTMLElement, KeyboardEvent, Node }
import scalatags.JsDom.all._
/**
 * Created by Peter Holzer on 18.09.2015.
 */

trait Editable  {
  def editDone(value:String,dir:Int):Unit
}

class CellEditor {
  val cell: TextArea = textarea(`class` := "cellinput", `type` := "text").render
  var justOpened=false
  /*org.scalajs.dom.document.onkeydown= {key:KeyboardEvent=>{
    key.keyCode match {
      case 27=> println("hallo")
      case _=>
    }
  }}*/
  org.scalajs.dom.window.onresize = (_: Event) => {
    if (isEditing) {
    if(justOpened)justOpened=false
    else cancelEdit()
    }
  }
  //org.scalajs.dom.document.addEventListener("blur",(e:Event)=>{println("focusout: src:"+e.srcElement+" t:" +e.target);SelectionController.printMessage("focusout: src:"+e.srcElement+" t:" +e.target)},true)
  cell.onkeydown= {key:KeyboardEvent=>
    //SelectionController.printMessage("key:"+key.keyCode)
    key.keyCode match {
      case 13=> if(key.getModifierState("Control")){
        val pos=cell.selectionStart
        val ctext=cell.value
        cell.value=ctext.substring(0,pos)+"\n"+ctext.substring(pos,ctext.length)
        cell.setSelectionRange(pos+1,pos+1)
      }  else {key.stopPropagation();finishEdit(0)} // enter
      case 37=> if(cell.selectionStart>0)key.stopPropagation()
      case 35|36|38|40=> key.stopPropagation()
      case 39=> if(cell.selectionEnd<cell.value.size)key.stopPropagation()

      case 27=> cancelEdit()  // esc
      case _=>
    }}

  //cell.addEventListener("blur",(e:Event)=>{finishEdit(0)})


  var callBack:Option[(String, Int) =>Unit]=None
  var parentNode:Option[Node]=None
  var oldChild:Option[Node]=None

  def startEdit(text:String,pNode:Node,editDone:(String,Int)=>Unit):Unit= {
    if(pNode==null) println("Pnode = null")
    else {
      if (parentNode.isDefined) cancelEdit()
      val width=pNode.asInstanceOf[HTMLElement].clientWidth
      cell.rows=if(text.length<5) 1 else if (text.length<10) 2 else if (text.length<50) 3 else 4
      println("edit width:"+width)
      cell.value = text
      callBack = Some(editDone)
      oldChild = Some(pNode.firstChild)
      justOpened=true
      pNode.removeChild(pNode.firstChild)
      pNode.appendChild(cell)
      cell.style.setProperty("width",(width-4)+"px")
      cell.style.setProperty("height","")
      cell.focus()
      parentNode = Some(pNode)

    }
  }

  def cancelEdit():Unit={
    justOpened=false
    for(p<-parentNode) {
      if(p!=null) {
        if(p.hasChildNodes())
        p.removeChild(cell)
        cell.blur()
        for (o <- oldChild){
          println("old child "+o)
          p.appendChild(o)
          p.asInstanceOf[HTMLElement].focus()
        }
      }
      parentNode=None
      callBack=None
      oldChild=None
    }
  }

  def finishEdit(dir:Int,focusTable:Boolean=true):Unit={
    justOpened=false
    for(p<-parentNode;c<-callBack) {
      val result=cell.value
      if(p.hasChildNodes())
        p.removeChild(cell)
      cell.blur()
      callBack=None
      parentNode=None
      document.getSelection().removeAllRanges()
      for (o <- oldChild){
        p.appendChild(o)
        if(focusTable) o.asInstanceOf[HTMLElement].focus()
      }
      oldChild=None
      c(result,dir)
    }
  }

  def isEditing: Boolean = parentNode.isDefined
}
