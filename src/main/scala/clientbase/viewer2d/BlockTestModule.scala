package clientbase.viewer2d

import clientbase.connection.{Subscriber, WebSocketConnector}
import clientbase.tableview.PluginModule
import definition.data.{BlockData, OwnerReference, Reference}
import definition.typ.AllClasses
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import util.StrToInt

import scala.scalajs.js.annotation.JSExportTopLevel


@JSExportTopLevel("BlockTestModule") class BlockTestModule extends PluginModule {

  var parentRef:Reference= _

  val headerNode: Div = div(`class` := "prfielddiv")("Testikel").render
  val blockTable= table(tr(th("ID"),th("data"))).render
  val createButton=button(`class` := "prfielddiv-createbutton", onclick := { () => createBlock })("create")
  val changeButton=button(`class` := "prfielddiv-createbutton", onclick := { () => changeBlock })("change")
  val deleteButton=button(`class` := "prfielddiv-createbutton", onclick := { () => deleteBlock })("delete")
  val buttonDiv= div(`class` := "prfielddiv")(createButton,changeButton,deleteButton).render
  val editField = input(`type`:="text").render
  val subscriber=new BlockSubscriber(AllClasses.get.blockClassList(1),update)

  val rowDiv: Div = div(id:="rowdiv",overflow := "hidden", clear := "both")(headerNode,blockTable,buttonDiv,editField).render

  override def moduleName: String = "Test"
  override def fullSize: Boolean = false
  override def content: HTMLElement = rowDiv
  override def shutDown(): Unit = {}

  override def load(ref: Reference): Unit = {
    //headerNode.innerHTML="Tester  <b>"+ref+"</b>"
    parentRef=ref
    println("load "+ref)
    WebSocketConnector.createBlockSubscription(ref,0,subscriber)
  }

  def clearTableRows(): Unit = {
    val numChild = blockTable.childElementCount
    for (_ <- 1 until numChild)
      blockTable.removeChild(rowDiv.children(1))
  }

  def update(): Unit ={
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
        WebSocketConnector.createBlock(1,OwnerReference(0,parentRef),array,c=>{println("created id= "+c.toInt)})
      case _=>
    }
  }

  def deleteBlock():Unit = {

  }

  def changeBlock():Unit= {

  }
}