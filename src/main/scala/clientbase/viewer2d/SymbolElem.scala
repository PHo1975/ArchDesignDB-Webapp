package clientbase.viewer2d

import definition.data.Reference
import definition.expression._
import org.denigma.threejs.{Camera, Object3D}

import scala.scalajs.js

class SymbolElem(nref:Reference,ncolor:Int,stampRef:Reference,val angle:Double,val scale:Double,pos:VectorConstant,paramString:String)
  extends GraphElem(nref,ncolor) {
  protected val _geometry=new js.Array[Object3D]
  val name: String ="S"+nref.instance
  var elems:Seq[GraphElem]=Seq.empty
  var _elemContainer:ElemContainer=_
  var _bounds:BRect=GraphElem.NULLRECT
  //println("Symbol ref:"+nref+" stamp:"+stampRef+" angle:"+angle+" pos:"+pos)
  StampPool.loadSymbol(stampRef,angle,pos, (data: Seq[GraphElem]) =>{
    elems=data
    if(_elemContainer!=null) createGeometry(_elemContainer)
  })

  protected var _pointsIterable:Iterable[VectorConstant]= _
  override def getBounds(container: ElemContainer): Bounds = _bounds

  override def createGeometry(container: ElemContainer): Unit = {
    //println("symbol "+ref+" createGeometry "+_elemContainer)
    _elemContainer=container

    if(elems.nonEmpty) {
      //println("createGeo "+nref+" angle:"+angle+" el:"+elems.size+" pos:"+pos)
      for (el <- elems) {
        el.createGeometry(container)
        for (obj: Object3D <- el.geometry) {
          //obj.position.x = obj.position.x + pos.x
          //obj.position.y = obj.position.y + pos.y
          obj.name=name
          _geometry.push(obj)
        }
      }
      _bounds=calcBounds
      //println("Symbol "+ref+" stamp:"+stampRef+" angle:"+angle+" _bounds:"+_bounds)
      container.dataUpdated()
    }
  }

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit = {
    var minX=Double.MaxValue
    var minY=Double.MaxValue
    var maxX=Double.MinValue
    var maxY=Double.MinValue
    for(el<-elems) el match {
      case _: GraphElemStub =>
      case g: GraphElem =>
        g.calcScreenBounds(container,camera,res)
        if(!res.isEmpty) {
          if (res.minX < minX) minX = res.minX
          if (res.minY < minY) minY = res.minY
          if (res.maxX > maxX) maxX = res.maxX
          if (res.maxY > maxY) maxY = res.maxY
        }
    }
    res.minX=minX
    res.maxX=maxX
    res.minY=minY
    res.maxY=maxY
  }

  def calcBounds:BRect = {
    var x1:Double=Short.MaxValue
    var y1:Double=Short.MaxValue
    var x2:Double=Short.MinValue
    var y2:Double=Short.MinValue
    for(el<-elems) el match {
      case _:GraphElemStub=>
      case g:GraphElem=>
        val l=g.getBounds(_elemContainer)
        if(l.minX!=0 || l.minY!=0 || l.maxX!=0 || l.maxY !=0 ) {
          if (l.minX < x1) x1 = l.minX
          if (l.minY < y1) y1 = l.minY
          if (l.maxX > x2) x2 = l.maxX
          if (l.maxY > y2) y2 = l.maxY
        }
    }
    BRect(x1,y1,x2,y2)
  }

  override def geometry: js.Array[Object3D] = _geometry

  override def getFormatFieldValue(fieldNr: Int): Constant = fieldNr match {
    case 0=> IntConstant(color)
    case 2=> new DoubleConstant(angle)
    case 3=> new DoubleConstant(scale)
    case _ =>EMPTY_EX
  }

  override def showSelection(): Unit = for(el<-elems) el.showSelection()


  override def hideSelection(): Unit = for(el<-elems) el.hideSelection()

  def createPointsIterable(container: ElemContainer): Iterable[VectorConstant] =
    if (_pointsIterable==null) {
      _pointsIterable=new Iterable[VectorConstant] {
        def iterator: Iterator[VectorConstant] = if (elems.isEmpty) Nil.iterator else  new Iterator[VectorConstant]{
          val elemIterator: Iterator[GraphElem] =elems.iterator
          var currentElem: GraphElem =elemIterator.next()
          var currentIterator:Iterator[VectorConstant]=currentElem.getHitPoints(container).iterator

          override def hasNext: Boolean = if (currentIterator.hasNext) true
          else {
            var hasPoints=false
            while(elemIterator.hasNext && ! hasPoints) {
              currentElem=elemIterator.next()
              currentIterator=currentElem.getHitPoints(container).iterator
              hasPoints= currentIterator.hasNext
            }
            hasPoints
          }

          override def next(): VectorConstant = currentIterator.next
        }
      }
      _pointsIterable
    } else _pointsIterable

  override def getHitPoints(container: ElemContainer): Iterable[VectorConstant] = createPointsIterable(container)
}
