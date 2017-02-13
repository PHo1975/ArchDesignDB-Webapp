package clientbase.viewer2d

import clientbase.connection.WebSocketConnector
import definition.data.Referencable

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Peter Holzer on 11.02.2017.
  */
class LayerList(controller:Viewer2DController) {

  val subscriberList: ArrayBuffer[LayerSubscriber] =ArrayBuffer[LayerSubscriber]()
  var loaded=false

  def loadLayers(nlist:Iterable[Referencable ]):Unit= {
    shutDown()
    if(nlist.isEmpty) controller.readyLoaded()
    else {
      val layerList=nlist.filter(_.ref.typ==GraphElem.LAYERTYPE).toIndexedSeq
      println("LayerList "+layerList.mkString("|"))
      val numLayer=layerList.size
      var currentLayer=0

      def loadNextLayer():Unit= {
        println("load next layer curr:"+currentLayer+" num:"+numLayer)
        if(currentLayer<numLayer) {
          val layer=layerList(currentLayer).ref
          val subscriber=new LayerSubscriber(layer,controller)
          subscriberList+= subscriber
          currentLayer+=1
          println("load "+layer)
          subscriber.load(layer,0,loadNextLayer _)
        } else readyLoaded()
      }

      loadNextLayer()
    }
  }

  def calcBounds(): BRect ={
    println("Calcbounds "+subscriberList.size)
    var x1=Double.MaxValue
    var y1=Double.MaxValue
    var x2=Double.MinValue
    var y2=Double.MinValue
    for(el<-subscriberList;if el.visible )  {
      val l=el.bounds
      if(l.minX<x1) x1=l.minX
      if(l.minY<y1) y1=l.minY
      if(l.maxX>x2) x2=l.maxX
      if(l.maxY>y2) y2=l.maxY
    }
    //println("bounds "+x1+"|"+y1+" - "+x2+"|"+y2)
    BRect(x1,y1,x2,y2)
  }

  def readyLoaded():Unit={
    println("ready loaded")
    loaded=true
    controller.readyLoaded()
  }

 def shutDown():Unit= {
    for (s<-subscriberList) s.unsubscribe()
    subscriberList.clear()
 }


}