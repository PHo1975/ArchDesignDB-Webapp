package clientbase.page

import clientbase.connection.WebSocketConnector
import clientbase.control.SidepanelController
import clientbase.tilelayout.{ ContentFactory, Tile }
import clientbase.viewer2d.Viewer2DController
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLElement
import util.Log
import scala.scalajs.js.JSApp
import scala.util.control.NonFatal


/**
 * Created by Peter Holzer on 05.04.2015.
 */
object Main extends JSApp {
  override def main(): Unit = {
    //println("main ready "+window.location.href+"|"+window.location.pathname)

    try {
      val mainPanel = document.getElementById("mainpanel").asInstanceOf[HTMLElement]
      //mainPanel.innerHTML="2"
      val statusLine = document.getElementById("statusline").asInstanceOf[HTMLElement]
      statusLine.innerHTML = "2"
      WebSocketConnector.start("WebTab", () => {
        document.body.removeChild(statusLine)
        val sidepanelRoot = document.getElementById("sidepanel").asInstanceOf[HTMLElement]
        SidepanelController.setup(sidepanelRoot, mainPanel)
        Tile.factoryList = List(ContentFactory("Z", "Zeichnung", () => new Viewer2DController()), ContentFactory("T", "Tabelle", () => new TableContent()))

        val topContent = new TableContent
        val topTile = new Tile(None, true)
        SidepanelController.switchListener += (()=>{topTile.notifyResize()})
        topTile.setSingleContent(topContent)
        topTile.setEdgeBottom(true)
        topTile.setEdgeRight(true)
        mainPanel.appendChild(topTile.myDiv)

        /*window.onpopstate=(event:PopStateEvent) => {
          event.state.toString match {
            case Reference(ref)=>pathMod.unsubscribe()
              WebSocketConnector.createPathSubscription(ref,pathMod)
          }
        }*/
      })
    } catch {
      case NonFatal(e) => println(e);Log.e(e)
    }
  }


}
