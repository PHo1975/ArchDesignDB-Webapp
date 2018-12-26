package clientbase.viewer2d

import java.io.DataInput

import clientbase.connection.WebSocketConnector
import clientbase.localstore.{SingleInstanceSubs, SubsMap}
import definition.data.{DimensionPoint, InstanceData, OwnerReference, Reference}
import definition.expression.Expression
import definition.typ.SelectGroup
import org.scalajs.dom.html.{Button, Image, TableCell, TableRow}
import org.scalajs.dom.raw.MouseEvent
import util.Log

import scala.scalajs.js
import scala.util.control.NonFatal
import scalatags.JsDom.all._


/**
  * Created by Peter Holzer on 11.02.2017.
  */
class LayerSubscriber(val layerRef: Reference, controller: Viewer2DController) extends SubsMap[GraphElem] {
  var visible = false
  var editable = false
  var scaleID:Int=1
  val nameCell: TableCell = td(onclick:= {_:MouseEvent=>forceActive()}).render
  val numCell: TableCell = td().render
  val scaleCell: TableCell = td().render
  val eyeIcon: TableCell = td(onclick := { _: MouseEvent => toggleVisibility() })(img(src := "eye.gif")).render
  val editIcon: TableCell = td(onclick := { _: MouseEvent => toggleEditable() })(img(src := "editsmall.gif")).render
  val newElemIcon: Image = img(src := "newElem.gif").render
  val newElemPlace: TableCell = td(onclick := { (_: MouseEvent) => setActive() }).render

  val removeBut: Button = button(onclick := { (_: MouseEvent) =>
    controller.layerList.removeLayer(this)  })("X").render

  val row: TableRow = if (WebSocketConnector.editable) tr(`class` := "layertable-row")(eyeIcon, editIcon, newElemPlace, numCell, nameCell, scaleCell, removeBut).render
                      else tr(`class` := "layertable-row")(eyeIcon, numCell, nameCell, scaleCell, removeBut).render

  val instSubscriber = new SingleInstanceSubs((inst) => {
    //println("Insubscriber update "+inst)
    numCell.innerHTML = inst.fieldValue(0).toString
    nameCell.innerHTML = inst.fieldValue(1).toString
    scaleCell.innerHTML = controller.scaleToString(ScaleModel.scales.getOrElse(inst.fieldValue(2).toInt, 0d))
    scaleID=inst.fieldValue(2).toInt
  })

  lazy val ownerReference = new OwnerReference(0.toByte, layerRef)

  override def factory(in: DataInput): GraphElem = try {
      val ref = Reference(in)
      ref.typ match {
        case GraphElem.LINETYPE =>
          val nfields = in.readByte
          //print("create Line "+ref+" fields:"+nfields)
          if (nfields != 5) util.Log.e("Line wrong number of fields " + nfields + " " + ref)
          val color = Expression.readConstant(in)
          val lineWidth = Expression.read(in).getValue
          val lineStyle = Expression.read(in).getValue
          val startPoint = Expression.read(in).getValue.toVector
          val endPoint = Expression.read(in).getValue.toVector
          val owners = InstanceData.readOwners(in)
          InstanceData.readSecondUseOwners(in)
          in.readBoolean
          LineElement(ref, color.toInt, lineWidth.toInt, lineStyle.toInt, startPoint, endPoint)

        case GraphElem.ARCTYPE =>
          val nfields = in.readByte
          if (nfields != 7) util.Log.e("Arc wrong number of fields " + nfields + " " + ref)
          val color = Expression.read(in).getValue
          val lineWidth = Expression.read(in).getValue
          val lineStyle = Expression.read(in).getValue
          val centerPoint = Expression.read(in).getValue.toVector
          val diameter = Expression.read(in).getValue.toDouble
          val startA = Expression.read(in).getValue.toDouble
          val endA = Expression.read(in).getValue.toDouble
          val owners = InstanceData.readOwners(in)
          InstanceData.readSecondUseOwners(in)
          in.readBoolean
          ArcElement(ref, color.toInt, lineWidth.toInt, lineStyle.toInt, centerPoint, diameter, startA, endA)

        case GraphElem.ELLIPSETYP ⇒
          val nfields = in.readByte
          if (nfields != 9) util.Log.e("Ellipse wrong number of fields " + nfields + " " + ref)
          val color = Expression.read(in).getValue
          val lineWidth = Expression.read(in).getValue
          val lineStyle = Expression.read(in).getValue
          val centerPoint = Expression.read(in).getValue.toVector
          val r1 = Expression.read(in).getValue.toDouble
          val r2 = Expression.read(in).getValue.toDouble
          val mainAngle = Expression.read(in).getValue.toDouble
          val startA = Expression.read(in).getValue.toDouble
          val endA = Expression.read(in).getValue.toDouble
          val owners = InstanceData.readOwners(in)
          InstanceData.readSecondUseOwners(in)
          in.readBoolean
          EllipseElement(ref, color.toInt, lineWidth.toInt, lineStyle.toInt, centerPoint, r1, r2, mainAngle, startA, endA)
        case GraphElem.TEXTTYP =>
            val nfields=in.readByte
            if(nfields!=10) util.Log.e("Line wrong number of fields "+nfields+" "+ref)
            val color=Expression.read(in).getValue.toInt
            val text=Expression.read(in).getValue.toString
            val pos=Expression.read(in).getValue.toVector
            val font=Expression.read(in).getValue.toString
            val height=Expression.read(in).getValue.toDouble
            val widthr=Expression.read(in).getValue.toDouble
            val align=Expression.read(in).getValue.toInt
            val tangle=Expression.read(in).getValue.toDouble
            val obAngle=Expression.read(in).getValue.toDouble
            val lineSpace=Expression.read(in).getValue.toDouble
            InstanceData.readOwners(in)
            InstanceData.readSecondUseOwners(in)
            in.readBoolean
            TextElement(ref,color,text,pos,font,height,widthr,align,tangle,obAngle,lineSpace)
        case GraphElem.DIMLINETYP =>
            val nfields=in.readByte
            if(nfields!=8) util.Log.e("DimLine wrong number of fields "+nfields+ " "+ref)
            else println("Dimline "+nfields)
            val color=Expression.read(in).getValue.toInt
            val position=Expression.read(in).getValue.toVector
            val style=Expression.read(in).getValue.toInt
            val angle=Expression.read(in).getValue.toDouble
            val points=DimensionPoint.createDimLineList(in)
            val refPoint=Expression.read(in).getValue.toVector
            val refDist=Expression.read(in).getValue.toDouble
            val precision=Expression.read(in).getValue.toDouble
            InstanceData.readOwners(in)
            InstanceData.readSecondUseOwners(in)
            in.readBoolean
            new DimLineElement(ref,color,position,style,angle,refPoint,refDist,precision,points)

        case _ => InstanceData.readWithChildInfo(ref, in); new GraphElemStub(ref)
      }
    } catch {
      case NonFatal(e)  => Log.e("factory ",e);null
  }

  override def update(it:Iterator[GraphElem]):Unit =
    controller.canvasHandler.repaint()

  /*override def onLoad(data: Iterator[GraphElem]): Unit = {
    for (el <- data)
      el.createGeometry(controller)
    super.onLoad(data)
    Log.w("Onload:"+map.size)
  }*/

  override def onUpdate(data: Iterator[GraphElem]):Unit = {
    for(el <- map.valuesIterator;o<-el.geometry)
      controller.canvasHandler.removeGeometry(o)
    super.onUpdate(data)
    for (el <- map.valuesIterator)
      el.createGeometry(controller)
    controller.dataUpdated()
  }

  override def onChange(data: GraphElem): Unit = {
    for (oldElem <- map.get(data.ref);o<-oldElem.geometry)
      controller.canvasHandler.removeGeometry(o)
    data.createGeometry(controller)
    super.onChange(data)
  }

  override def destructor(elem: GraphElem): Unit = {
    for(o<-elem.geometry)
    controller.canvasHandler.removeGeometry(o)
    super.destructor(elem)
  }

  override def onChildAdded(data: GraphElem): Unit = {
    data.createGeometry(controller)
    super.onChildAdded(data)
  }


  def bounds:BRect= {
    var x1:Double=Short.MaxValue
    var y1:Double=Short.MaxValue
    var x2:Double=Short.MinValue
    var y2:Double=Short.MinValue
    for(el<-map.valuesIterator) el match {
      case _:GraphElemStub=>
      case g:GraphElem=>
        val l=g.getBounds(controller)
        if(l.minX<x1) x1=l.minX
        if(l.minY<y1) y1=l.minY
        if(l.maxX>x2) x2=l.maxX
        if(l.maxY>y2) y2=l.maxY
    }
    BRect(x1,y1,x2,y2)
  }

  def toggleVisibility(): Unit = controller.layerList.toggleVisibility(this)

  def toggleEditable(): Unit = controller.layerList.toggleEditable(this)

  def setActive():Unit = controller.layerList.setActiveLayer(this)

  def forceActive():Unit = controller.layerList.forceActiveLayer(this)


  def show(): Unit = if (!visible && subsID == -1) {
    println("show "+layerRef)
    load(layerRef, 0, () => {
      eyeIcon.style.background = "blue"
      controller.zoomAll()
    })
    visible = true
  }

  def hide(): Unit = if (visible && subsID != -1) {
    val handler = controller.canvasHandler
    for (el <- map.valuesIterator;o<-el.geometry)
      handler.removeGeometry(o)
    unsubscribe()
    map.clear()
    controller.canvasHandler.repaint()
    eyeIcon.style.background = "white"
    visible = false
    setEditable(false)
  }

  def setEditable(edit: Boolean): Unit = {
    editable = edit
    editIcon.style.background = if (edit) "blue" else "white"
  }

  def setActive(act: Boolean): Unit = {
    newElemPlace.style.background = if (act) "blue" else "white"
    if(act) newElemPlace.appendChild(newElemIcon)
    else newElemPlace.removeChild(newElemIcon)
    controller.scaleModel.setRelativeScaleID(scaleID)
  }

  def filterSelection(intersection: js.Array[Reference]): Option[SelectGroup[GraphElem]] = {
    val elemsInLayer: js.Array[GraphElem] = intersection.filter(map.contains).map(map)
    println("layer " + layerRef + " elems:" + elemsInLayer.mkString(" ,"))
    if (elemsInLayer.length == 0) None
    else Some(SelectGroup(ownerReference, elemsInLayer))
  }

}
