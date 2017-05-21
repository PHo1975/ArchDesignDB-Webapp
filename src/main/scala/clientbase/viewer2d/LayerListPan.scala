package clientbase.viewer2d

import clientbase.connection.WebSocketConnector
import org.scalajs.dom.html.{ Div, Table, TableRow }
import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 27.02.2017 .
  */
class LayerListPan(controller: Viewer2DController) {
  var visible = true
  val headerRow: TableRow = if (WebSocketConnector.editable) tr(th("Sic"), th("Bea"), th("Neu"), th("Nr."), th("Name"), th("Mass"), th("Weg")).render
                            else tr(th("Sic"), th("Nr."), th("Name"), th("Mass"), th("Weg")).render
  val layerTab: Table = table(`class` := "layerlisttable")(headerRow).render
  val pane: Div = div(`class` := "layerlistpan")(layerTab).render

  def addLayer(newLayer: LayerSubscriber): Unit = {

    layerTab.appendChild(newLayer.row)
  }

  def toggleVisibility(): Unit = {
    if (visible) pane.removeChild(layerTab) else pane.appendChild(layerTab)
    visible = !visible
    controller.canvasHandler.onResize()
  }

  def removeLayer(ix: Int): Unit = layerTab.removeChild(layerTab.children(ix + 1))
}
