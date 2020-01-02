package clientbase.viewer2d

import definition.data.{Referencable, Reference}
import definition.expression.{NULLVECTOR, VectorConstant}
import definition.typ.SelectGroup
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

/**
  * Created by Peter Holzer on 11.02.2017.
  */
class LayerList(controller:Viewer2DController) {

  val subscriberList: ArrayBuffer[LayerSubscriber] =ArrayBuffer[LayerSubscriber]()
  var loaded=false
  var activeLayer: Option[LayerSubscriber] = None

  val tempElements: ArrayBuffer[GraphElem] =ArrayBuffer[GraphElem]()

  def loadLayers(nlist:Iterable[Referencable ]):Unit= {
    shutDown()
    if(nlist.isEmpty) controller.readyLoaded()
    else {
      val layerList=nlist.filter(_.ref.typ==GraphElem.LAYERTYPE).toIndexedSeq
      val numLayer=layerList.size
      var currentLayer=0

      def loadNextLayer():Unit= {
        //println("load next layer curr:"+currentLayer+" num:"+numLayer)
        if(currentLayer<numLayer) {
          val layer=layerList(currentLayer).ref
          val subscriber=new LayerSubscriber(layer,controller)
          subscriberList+= subscriber
          currentLayer+=1
          //println("load "+layer)
          subscriber.instSubscriber.load(layer, -1, Some(loadNextLayer _))
        } else readyLoaded()
      }

      loadNextLayer()
    }
  }

  def setActiveLayer(lay: LayerSubscriber): Unit = {
    for(ol<-activeLayer) ol.setActive(false)
    activeLayer = Some(lay)
    lay.show()
    lay.setEditable(true)
    lay.setActive(true)
    controller.canvasHandler.notifyFocus()
  }

  def forceActiveLayer(lay:LayerSubscriber):Unit= {
    for(l<-subscriberList;if l.visible) l.hide()
    setActiveLayer(lay)
  }

  def toggleVisibility(lay: LayerSubscriber): Unit = {
    if (lay.visible) {
      lay.hide()
      lay.setEditable(false)
      for (f <- activeLayer; if f == lay) {
        f.setActive(false)
        activeLayer = None
      }
    } else lay.show()
  }

  def toggleEditable(lay: LayerSubscriber): Unit = {
    if (lay.editable) {
      for (f <- activeLayer; if f == lay) {
        f.setActive(false)
        activeLayer = None
      }
      lay.hide()
    } else if(!lay.visible) lay.show()
    lay.setEditable(!lay.editable)
  }

  def editableLayers: Iterator[LayerSubscriber] = subscriberList.iterator.filter(_.editable)


  def calcBounds(): BRect ={
    //println("Calcbounds "+subscriberList.size)
    var x1:Double=Short.MaxValue
    var y1:Double=Short.MaxValue
    var x2:Double=Short.MinValue
    var y2:Double=Short.MinValue
    for(el<-subscriberList;if el.visible )  {
      val l=el.bounds
      if(l.minX<x1) x1=l.minX
      if(l.minY<y1) y1=l.minY
      if(l.maxX>x2) x2=l.maxX
      if(l.maxY>y2) y2=l.maxY
    }
    //Log.w("bounds "+x1+"|"+y1+" - "+x2+"|"+y2)
    BRect(x1,y1,x2,y2)
  }

  def readyLoaded():Unit={
    println("ready Loaded"+subscriberList.size)
    loaded=true
    for (l <- subscriberList.headOption) setActiveLayer(l)
    controller.readyLoaded()
  }

 def shutDown():Unit= {
    for (s<-subscriberList) s.unsubscribe()
    subscriberList.clear()
 }

  def removeLayer(l: LayerSubscriber): Unit = {
    subscriberList.indexWhere(_.layerRef == l.layerRef) match {
      case -1 => Log.e("removeLayer not possible " + l.layerRef)
      case ix =>
        l.hide()
        subscriberList(ix).unsubscribe()
        subscriberList.remove(ix)
        controller.layerPan.removeLayer(ix)
    }
  }

  /**
  * creates a List of SelectGroup Objects for each Layer containing Objects from the intersection list
*/
  def decodeSelection(intersection: js.Array[Reference]): Seq[SelectGroup[GraphElem]] =
    editableLayers.flatMap(_.filterSelection(intersection)).toSeq


  def filterElements(onlyEdible:Boolean,filterFunc:GraphElem=>Boolean):Iterable[SelectGroup[GraphElem]]= {
    val ret1 = subscriberList.flatMap(a=>
      if(onlyEdible && !a.editable) Seq.empty
      else {
        val list: Iterable[GraphElem] =a.filterElements(filterFunc)
        if (list.isEmpty) Seq.empty
        else List(SelectGroup(a.ownerReference,list))
    } )
    ret1
  }

  def iterateVisiblePoints(container:ElemContainer,func:VectorConstant=>Unit):Unit = {
    func(NULLVECTOR)
    for(layer<-subscriberList;if layer.visible) layer.iterateVisiblePoints(container,func)
  }

}
