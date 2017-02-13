package clientbase.viewer2d

import java.io.DataInput

import clientbase.connection.Subscriber
import clientbase.localstore.SubsMap
import definition.data.{InstanceData, Reference}
import definition.expression.Expression
import util.Log

import scala.util.control.NonFatal

/**
  * Created by Peter Holzer on 11.02.2017.
  */
class LayerSubscriber(layerRef:Reference,controller:Viewer2DController) extends SubsMap[GraphElem] {
  var visible=true
  override def factory(in: DataInput): GraphElem = {

    try {
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

        case _ => InstanceData.readWithChildInfo(ref, in); new GraphElemStub(ref)
      }
    } catch {
      case NonFatal(e)  => Log.e("factory ",e);null
    }
  }

  override def update(data: Iterator[GraphElem]):Unit = {
    controller.dataUpdated()
  }


  def bounds:BRect= {
    var x1=Double.MaxValue
    var y1=Double.MaxValue
    var x2=Double.MinValue
    var y2=Double.MinValue
    for(el<-map.valuesIterator) el match {
      case n:GraphElemStub=>
      case g:GraphElem=>
        val l=g.getBounds(controller)
        if(l.minX<x1) x1=l.minX
        if(l.minY<y1) y1=l.minY
        if(l.maxX>x2) x2=l.maxX
        if(l.maxY>y2) y2=l.maxY
    }
    BRect(x1,y1,x2,y2)
  }

}
