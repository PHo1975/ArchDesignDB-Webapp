package clientbase.viewer2d

import clientbase.connection.{Subscriber, WebSocketConnector}
import clientbase.tableview.PluginModule
import definition.data.{BlockData, OwnerReference, Reference}
import definition.typ.AllClasses
import org.scalajs.dom.html.{Button, Div, Input, Table}
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom
import scalatags.JsDom.all._
import util.StrToInt

import scala.scalajs.js.annotation.JSExportTopLevel


@JSExportTopLevel("BlockTestModule") class BlockTestModule extends PluginModule {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  var parentRef:Reference= _

  val headerNode: Div = div(`class` := "prfielddiv")("Testikel").render
  val blockTable: Table = table(tr(th("ID"),th("data"))).render
  val createButton =button(`class` := "prfielddiv-createbutton", onclick := { () => createBlock })("create")
  val changeButton =button(`class` := "prfielddiv-createbutton", onclick := { () => changeBlock })("change")
  val deleteButton=button(`class` := "prfielddiv-createbutton", onclick := { () => deleteBlock })("delete")
  val clearButton=button(`class` := "prfielddiv-createbutton", onclick := { () => clearTableRows() })("clear")
  val buttonDiv: Div = div(`class` := "prfielddiv")(createButton,changeButton,deleteButton,clearButton).render
  val editField: Input = input(`type`:="text").render
  val subscriber=new BlockSubscriber(AllClasses.get.blockClassList(1),update)

  val rowDiv: Div = div(id:="rowdiv",overflow := "hidden", clear := "both")(headerNode,blockTable,buttonDiv,editField).render

  override def moduleName: String = "Test"
  override def fullSize: Boolean = false
  override def content: HTMLElement = rowDiv
  override def shutDown(): Unit = {
    println("shutdown parentRef:"+parentRef)
    subscriber.unsubscribe()
  }

  override def load(ref: Reference): Unit = {
    //headerNode.innerHTML="Tester  <b>"+ref+"</b>"
    parentRef=ref
    println("load "+ref)
    WebSocketConnector.createBlockSubscription(ref,0,subscriber)
  }

  def clearTableRows(): Unit = {
    val numChild = blockTable.childElementCount
    for (_ <- 1 until numChild)
      blockTable.removeChild(blockTable.children(1))
  }

  def update(): Unit ={
    println("update")
    clearTableRows()
    for(block<-subscriber.blockList) {
      println("data:"+block.data.mkString("|"))
      blockTable.appendChild(tr(td(block.ref.toString),td(block.data.mkString("|"))).render)
    }
  }

  def createBlock(): Unit ={
    editField.value match {
      case StrToInt(value) if value<255 =>
        val array: Array[Byte] =(0 until 10).map(n=>(n +value).toByte).toArray
        println("createData "+array.mkString("|"))
        for(c<-WebSocketConnector.createBlock(1,OwnerReference(0,parentRef),array))
          println("created id= "+c)
      case _=>
    }
  }

  def deleteBlock():Unit = {
    editField.value match {
      case StrToInt(ix) if ix>= 0 && ix<subscriber.blockList.size=>
        val ref=subscriber.blockList(ix).ref
        WebSocketConnector.deleteBlock(ref,OwnerReference(0,parentRef))
      case o => println("Wrong index "+o)
    }
  }

  def changeBlock():Unit= {
    editField.value.split(",") match {
      case Array(StrToInt(ix),StrToInt(value)) if ix<subscriber.blockList.size=>
        val array: Array[Byte] =(0 until 10).map(n=>(n +value).toByte).toArray
        val ref=subscriber.blockList(ix).ref
        println("change ref:"+ref+" parentRef:"+parentRef+" value:"+value)
        WebSocketConnector.changeBlock(OwnerReference(0,parentRef),new BlockData(ref,array))
      case o => println("to change, enter: ix,value")
    }
  }

  override def updateResize(): Unit = {

  }
}