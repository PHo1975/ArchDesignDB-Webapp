package clientbase.page

import clientbase.connection.WebSocketConnector
import clientbase.control.SidepanelController
import clientbase.tableview.PathModel
import clientbase.tilelayout.{ContentFactory, Tile}
import clientbase.viewer2d.Viewer2DController
import definition.data.Reference
import org.scalajs.dom.raw.{HTMLElement, PopStateEvent}
import org.scalajs.dom.{document, window}
import util.Log

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.util.control.NonFatal


/**
 * Created by Peter Holzer on 05.04.2015.
 */
object Main extends JSApp {
  @JSExport
  override def main(): Unit = {
    //println("main ready "+window.location.href+"|"+window.location.pathname)

    try {
      WebSocketConnector.start("WebTab", () => {
        //println("sytemsettings loaded")
        val sidepanelRoot = document.getElementById("sidepanel").asInstanceOf[HTMLElement]
        val mainPanel = document.getElementById("mainpanel").asInstanceOf[HTMLElement]
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
