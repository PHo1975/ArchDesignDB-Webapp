package clientbase.page

import clientbase.connection.WebSocketConnector
import clientbase.control.SelectionController
import clientbase.tableview.PathModel
import clientbase.tilelayout.TileContent
import definition.data.{Referencable, Reference}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.window

import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 29.01.2017.
  */
class TableContent extends TileContent {
  def load():Unit={}
  def save():Unit={}
  val pathDiv: Div =div(`class`:="headerdiv").render
  val contentDiv: Div =div(style:="overflow:auto").render
  val content:HTMLElement =div(`class`:="table-tile")( pathDiv,contentDiv).render
  val pathMod: PathModel =  new PathModel(pathDiv, contentDiv)

  //println("Start TableContent "+getRoot)
  init(Nil)

  def init(selection:Iterable[Referencable]):Unit= {
    WebSocketConnector.createPathSubscription(if(selection.isEmpty) getRoot else
      selection.head.ref,pathMod)
  }
  def getSelection:Iterable[Referencable]={
    //println("Get selection "+SelectionController.currentSelection.mkString)
    if(SelectionController.currentSelection.isEmpty) Nil
    else SelectionController.currentSelection.head.children
  }


  def getRoot:Reference={
    val params=window.location.href.split("\\?",2)
    if(params.length==2) {
      params(1) match {
        case Reference(ref)=> ref
        case _=> println("wrong parameter format "+params(1), "use: type,inst");WebSocketConnector.root
      }
    } else WebSocketConnector.root
  }

  def close():Unit = {WebSocketConnector.removeSubscription(pathMod.subsID)}
}
