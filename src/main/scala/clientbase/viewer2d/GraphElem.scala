package clientbase.viewer2d

import definition.data.{Referencable, Reference}
import definition.expression._
import org.denigma.threejs._
import util.Log

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.{Float32Array, Float64Array}

/**
  * Created by Peter Holzer on 11.02.2017.
  */


trait Bounds{
  def minX:Double
  def minY:Double
  def maxX:Double
  def maxY:Double
}

case class BRect(minX:Double,minY:Double,maxX:Double,maxY:Double) extends Bounds

class BoundsContainer(var minX:Double=0d,var minY:Double=0d,var maxX:Double=0d,var maxY:Double=0d) extends Bounds {
  def isEmpty: Boolean = minX==Double.MinValue & minY == Double.MinValue && maxX==Double.MinValue & maxY == Double.MinValue
  def setEmpty():Unit = {
    minX=Double.MinValue
    minY=Double.MinValue
    maxX=Double.MinValue
    maxY=Double.MinValue
  }
}

trait Formatable extends Referencable {
  def getFormatFieldValue(fieldNr: Int): Constant
  def getHitPoints(container:ElemContainer):Iterable[VectorConstant]
}

trait ElemContainer {
  def scaleRatio:Double
  def addGeometry(geom:Object3D):Unit
  def dataUpdated():Unit
}


trait SelectionDecorable {
  def showSelection(): Unit

  def hideSelection(): Unit
}


abstract class GraphElem(override val ref: Reference, val color: Int) extends Formatable with SelectionDecorable {
  def getBounds(container: ElemContainer): Bounds
  def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit = res.setEmpty()

  def createGeometry(container:ElemContainer):Unit
  def geometry:js.Array[Object3D]

  def showSelection(): Unit = for(obj<-geometry) obj match {
    case m: Mesh => m.material = GraphElem.selectMaterial
    case _ =>
  }

  def hideSelection(): Unit = {
    println("hideselection "+ref)
    for(obj<-geometry) obj match {
    case m: Mesh => m.material = GraphElem.getMaterial(color)
    case _ =>
  }}
}

class GraphElemStub(override val ref:Reference) extends GraphElem(ref,0)  {
  def getFormatFieldValue(fieldNr:Int):Constant=EMPTY_EX
  def getBounds(container: ElemContainer): Bounds =GraphElem.NULLRECT
  def getHitPoints(container:ElemContainer):Iterable[VectorConstant]=Seq.empty

  def createGeometry(container:ElemContainer): Unit = {}
  //def hideSelection(): Unit = {}
  def geometry:js.Array[Object3D]=null
}

abstract class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor) {
  protected val _geometry= new js.Array[Object3D]
  def geometry: js.Array[Object3D] =_geometry

  override def getFormatFieldValue(fieldNr:Int):Constant= {
    fieldNr match {
      case 0 => IntConstant(color)
      case 1 => IntConstant(lineWidth)
      case 2 => IntConstant(lineStyle)
      case _ => null
    }
  }


}

abstract class AbstractLineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,val startPoint:VectorConstant,val endPoint:VectorConstant) extends
  LinearElement(nref,ncolor,nlineWidth,nlineStyle) with Bounds {
  val hitPoints=List(startPoint,endPoint)

  def minX: Double = scala.math.min(startPoint.x, endPoint.x)
  def minY: Double = scala.math.min(startPoint.y, endPoint.y)
  def maxX: Double = scala.math.max(startPoint.x, endPoint.x)
  def maxY: Double = scala.math.max(startPoint.y, endPoint.y)

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfrom2Points(startPoint,endPoint,camera,res)

  def getHitPoints(container:ElemContainer): List[VectorConstant] =hitPoints

  override def getBounds(container: ElemContainer): Bounds = this
}



// ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
case class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,nstartPoint:VectorConstant,nendPoint:VectorConstant) extends
  AbstractLineElement(nref,ncolor,nlineWidth,nlineStyle,nstartPoint,nendPoint) {

  def createGeometry(container:ElemContainer): Unit = {
    val mesh=GraphElem.createLineGeometry(container,startPoint,endPoint,lineWidth,color)
    mesh.name = "L" + ref.instance
    _geometry.push(mesh)
    container.addGeometry(mesh)
  }
}

// ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
                      diameter:Double,startAngle:Double,endAngle:Double) extends
  LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
  //println("create arc "+ref+" "+centerPoint)
  lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle),pointFromAngle(endAngle),centerPoint)
  def getHitPoints(container:ElemContainer): Seq[VectorConstant] =points
  protected lazy val bounds: BRect = calcArcBounds
  protected val cornerPoints: ArrayBuffer[VectorConstant] =ArrayBuffer[VectorConstant]()

  def deltaW: Double = {
    val value = endAngle - startAngle + (if (endAngle < startAngle) 360d else 0)
    if (value == 360d) 360d else value % 360d
  }

  def getBounds(container: ElemContainer): Bounds =bounds


  def pointFromAngle(angle:Double) =
    new VectorConstant(centerPoint.x+scala.math.cos(angle*scala.math.Pi/180d)*diameter,
      centerPoint.y+scala.math.sin(angle*scala.math.Pi/180d)*diameter,0)

  protected def createBoundsPoints: ArrayBuffer[VectorConstant] = {
    if(cornerPoints.isEmpty) {
      cornerPoints += points.head += points.tail.head
      val sa = startAngle % 360d
      var ea = (if (endAngle < sa) endAngle + 360d else endAngle) % 360d
      if (ea == sa) ea += 360d
      var nextSegmentAngle = ((scala.math.floor(sa / 90d) + 1d) * 90d) % 360d
      //System.out.println("startAngle "+sa+" "+nextSegmentAngle+" ea:"+ea)
      while (nextSegmentAngle < ea) {
        val np = pointFromAngle(nextSegmentAngle)
        //System.out.println("nxa:"+nextSegmentAngle+"Np "+np)
        cornerPoints += np
        nextSegmentAngle += 90d
      }
    }
    cornerPoints
  }

  def calcArcBounds: BRect = {
    GraphElem.getPointsBounds(createBoundsPoints)
  }

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfromPointList(createBoundsPoints.iterator,camera,res)

  override def createGeometry(container:ElemContainer): Unit = {
    try {
      val geom=GraphElem.createCircleGeometry(container,diameter,lineWidth,deltaW,GraphElem.schrittWeiteKreis)
      val mesh = new Mesh(geom, GraphElem.getMaterial(color))
      mesh.position.x = centerPoint.x
      mesh.position.y = centerPoint.y
      mesh.rotation.z = startAngle * Math.PI / 180d
      mesh.name = "C" + ref.instance
      _geometry.push(mesh)
      container.addGeometry(mesh)
    } catch {
      case e: Throwable => util.Log.e("Arc Geo:", e)
    }
  }
}




case class EllipseElement(nref: Reference, ncolor: Int, nlineWidth: Int, nlineStyle: Int, centerPoint: VectorConstant,
                          r1: Double, r2: Double, mainAngle: Double, startAngle: Double, endAngle: Double) extends LinearElement(nref, ncolor, nlineWidth, nlineStyle) {

  private def getInnerAngleFirstQuadrant(outerAngle: Double, delta: Double) =
    math.atan(math.tan(outerAngle) * (r1 + delta) / (r2 + delta))

  lazy val points: Seq[VectorConstant] = List(pointFromAngle(startAngle * math.Pi / 180d), pointFromAngle(endAngle * math.Pi / 180d), centerPoint)
  def getHitPoints(container:ElemContainer): Iterable[VectorConstant] =points
  protected lazy val bounds: Bounds = calcBounds
  protected val cornerPoints: ArrayBuffer[VectorConstant] =ArrayBuffer[VectorConstant]()

  def getInnerAngle(outerAngle: Double, delta: Double): Double =
    if (outerAngle > math.Pi / 2) {
      if (outerAngle > GraphElem.PI_32) getInnerAngleFirstQuadrant(outerAngle - math.Pi * 2, delta) + math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle - math.Pi, delta) + math.Pi
    }
    else if (outerAngle < -math.Pi / 2) {
      if (outerAngle < -GraphElem.PI_32) getInnerAngleFirstQuadrant(outerAngle + math.Pi * 2, delta) - math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle + math.Pi, delta) - math.Pi
    }
    else getInnerAngleFirstQuadrant(outerAngle, delta)

  def pointFromAngle(angle: Double, delta: Double = 0d): VectorConstant = {
    val ia = getInnerAngle(angle % GraphElem.PI2, delta)
    val dx = math.cos(ia) * (r1 + delta)
    val dy = math.sin(ia) * (r2 + delta)
    val ma = mainAngle * math.Pi / 180d
    val cosMa = math.cos(ma)
    val sinMa = math.sin(ma)
    new VectorConstant(dx * cosMa - dy * sinMa + centerPoint.x, dx * sinMa + dy * cosMa + centerPoint.y, 0)
  }

  def calcBounds: Bounds =  GraphElem.getPointsBounds(createBoundsPoints)

  protected def createBoundsPoints: ArrayBuffer[VectorConstant] = {
    if(cornerPoints.isEmpty) {
      cornerPoints += points.head += points.tail.head
      val ea = if (endAngle < startAngle) endAngle + 360 else endAngle
      var nextSegmentAngle = (scala.math.floor(startAngle / 90) + 1) * 90
      while (nextSegmentAngle < ea) {
        val np = pointFromAngle(nextSegmentAngle * math.Pi / 180d)
        cornerPoints += np
        nextSegmentAngle += 90
      }
    }
    cornerPoints
  }

  override def getBounds(container: ElemContainer): Bounds = bounds

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfromPointList(createBoundsPoints.iterator,camera,res)

  def deltaW: Double = {
    val value = endAngle - startAngle + (if (endAngle < startAngle) 360d else 0)
    if (value == 360d) 360d else value % 360d
  }

  override def createGeometry(container:ElemContainer): Unit = {
    try {
      val delta = deltaW
      val steps = Math.floor(delta / GraphElem.schrittWeiteEllipse).toInt
      val apoints = new Float32Array((steps + 2) * 3 * 2)
      val thick = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d

      def setPoints(index: Int, angle: Double): Unit = {
        val radAngle = (angle + startAngle) * Math.PI / 180d
        val p1 = pointFromAngle(radAngle, -thick)
        val p2 = pointFromAngle(radAngle, thick)
        apoints(index) = p1.x.toFloat
        apoints(index + 1) = p1.y.toFloat
        apoints(index + 3) = p2.x.toFloat
        apoints(index + 4) = p2.y.toFloat
      }
      for (st <- 0 to steps)
        setPoints(st * 6, st.toDouble * GraphElem.schrittWeiteEllipse)
      setPoints((steps + 1) * 6, delta)
      val geom = new MyBufferGeometry()
      val indices = new js.Array[Int]((steps + 1) * 6)
      var current = 0
      for (i <- 0 until (steps + 1) * 2; l <- 0 to 2) {
        indices(current) = if (i % 2 == 0) i + l else i + 2 - l
        current += 1
      }
      geom.setIndex(indices)
      geom.addAttribute("position", new BufferAttribute(apoints, 3))
      geom.computeFaceNormals()
      geom.computeBoundingSphere()
      val mesh = new Mesh(geom, GraphElem.getMaterial(color))
      mesh.name = "E" + ref.instance
      _geometry.push(mesh)
      container.addGeometry(mesh)
    } catch {
      case e: Throwable => util.Log.e("Ellipse Geo:", e)
    }
  }
}




case class TextElement(nref:Reference,ncolor:Int,text:String,position:VectorConstant,fontName:String,fheight:Double,
                       widthRatio:Double,style:Int,
                       textAngle:Double,obligeAngle:Double,lineSpace:Double) extends GraphElem(nref,ncolor) {
  protected val _geometry=new js.Array[Object3D]
  val meshName: String ="T"+ref.instance
  protected var textWidth: Double = -1d
  private var cornerPoints:Array[VectorConstant]=_

  protected def getCornerPoints(container:ElemContainer): Array[VectorConstant] =if (cornerPoints==null) {
    cornerPoints=new Array[VectorConstant](4)
    val height=fheight/1000d*container.scaleRatio
    val width=if(textWidth>0) textWidth else height*text.length/2
    val x=position.x+(if(isStyle(GraphElem.rightStyle))-textWidth
    else if(isStyle(GraphElem.hCenterStyle)) -textWidth/2 else 0)
    val y=position.y+ (if(isStyle(GraphElem.bottomStyle))-height
    else if (isStyle(GraphElem.vCenterStyle)) -height/2 else 0)
    cornerPoints(0)=new VectorConstant(x,y,0)
    cornerPoints(1)=new VectorConstant(x,y+height,0)
    cornerPoints(2)=new VectorConstant(x+width,y+height,0)
    cornerPoints(3)=new VectorConstant(x+width,y,0)
    cornerPoints
  } else cornerPoints

  def isStyle(pattern: Int): Boolean = (style & pattern) > 0

  override def geometry:js.Array[Object3D]=_geometry

  protected def calcBounds(container:ElemContainer):Bounds= {
    val height=fheight/1000d*container.scaleRatio
    val width=if(textWidth>0) textWidth/2 else height*text.length/2
    BRect(position.x-width,position.y-height/2,position.x+width,position.y+height/2)
  }

  override def getBounds(container: ElemContainer): Bounds = calcBounds(container)

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfromPointList(getCornerPoints(container).iterator,camera,res)

  override def createGeometry(container:ElemContainer): Unit = try{
    GraphElem.loadTextGeometry(container,text,fheight,fontName, (tgArray: js.Array[ShapeGeometry]) =>{
      if(tgArray.length>0) {
        tgArray.head.computeBoundingBox()
        tgArray.last.computeBoundingBox()
        textWidth = tgArray.last.boundingBox.max.x - tgArray.head.boundingBox.min.x
        val textHeight = fheight / 1000 * container.scaleRatio
        for (geom <- tgArray) {
          val mesh = new Mesh(geom, GraphElem.getMaterial(color))
          mesh.position.set(position.x + (if (isStyle(GraphElem.rightStyle)) -textWidth
          else if (isStyle(GraphElem.hCenterStyle)) -textWidth / 2 else 0),
            position.y + (if (isStyle(GraphElem.bottomStyle)) -textHeight
            else if (isStyle(GraphElem.vCenterStyle)) -textHeight / 2 else 0), 0f)
          mesh.rotation.z = textAngle * Math.PI / 180d
          mesh.name = meshName
          container.addGeometry(mesh)
          _geometry.push(mesh)
        }
      } else println("Emtpy Text Geometry for Text:"+text+" font:"+fontName)
    })
  }catch {
    case e: Throwable => util.Log.e("Font Geo:", e)
  }

  override def getFormatFieldValue(fieldNr: Int): Constant = fieldNr match {
    case 0 => IntConstant(color)
    case _ => null
  }

  override def getHitPoints(container: ElemContainer): Iterable[VectorConstant] = getCornerPoints(container)
}






/***************************************************************************************************************************************/

object GraphElem {
  final val LAYERTYPE = 39
  final val LINETYPE = 40
  final val ARCTYPE = 41
  final val FILLTYPE = 42
  final val ELLIPSETYP = 43
  final val TEXTTYP = 44
  final val SYMBOLTYP = 45
  final val DIMLINETYP= 46
  final val NULLRECT: BRect = BRect(0, 0, 0, 0)
  final val schrittWeiteKreis = 11.25d
  final val schrittWeiteEllipse: Double = schrittWeiteKreis / 2d
  final val PI_32: Double = math.Pi * 3d / 2d
  final val PI2: Double = math.Pi * 2d
  final val vCenterStyle = 1
  final val bottomStyle = 2
  final val hCenterStyle = 8
  final val rightStyle = 16
  val HITX: Byte = 1
  val HITY: Byte = 2
  val HITBOTH: Byte = 3

  var font:ThreeFont=_
  //val fontMap=mutable.HashMap[String,js.Any]()
  val fontLoader: FontLoader =new FontLoader()
  var isFontLoading:Boolean=false
  val fontLoadCallbacks= new ArrayBuffer[(ThreeFont,Boolean)=>Unit]()

  def createLineGeometry(container:ElemContainer,startPoint:VectorConstant,endPoint:VectorConstant,lineWidth:Double,color:Int): Mesh = {
      val delta = endPoint - startPoint
      val center: VectorConstant = startPoint + (delta * 0.5d)
      val lw = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d
      //val planeBuffer=new PlaneBufferGeometry(delta.toDouble+lw,lw)
      val mesh = new Mesh(GraphElem.lineGeometry, GraphElem.getMaterial(color))
      mesh.scale.x = delta.toDouble + lw
      mesh.scale.y = lw
      val rot = Math.atan2(delta.y, delta.x)
      mesh.position.x = center.x //-Math.cos(rot)*lw/2d
      mesh.position.y = center.y //-math.sin(rot)*lw/2d
      mesh.rotation.z = rot
      mesh
    }


    def createCircleGeometry(container:ElemContainer,diameter:Double,lineWidth:Double,delta:Double,schrittWeite:Double): MyBufferGeometry = {
      try {
        val steps = Math.floor(delta / schrittWeite).toInt
        //println(" delta:"+delta+" schrittWeite:"+schrittWeite+" steps:"+steps+" diameter:"+diameter+" linewidth:"+lineWidth)
        val apoints = new Float32Array((steps + 2) * 3 * 2)
        val thick = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d
        val d1 = diameter - thick
        val d2 = diameter + thick

        def setPoints(index: Int, angle: Double): Unit = {
          val radAngle = angle * Math.PI / 180d
          apoints(index) = (Math.cos(radAngle) * d1).toFloat
          apoints(index + 1) = (Math.sin(radAngle) * d1).toFloat
          apoints(index + 3) = (Math.cos(radAngle) * d2).toFloat
          apoints(index + 4) = (Math.sin(radAngle) * d2).toFloat
        }

        //setPoints(0, 0)
        for (st <- 0 to steps)
          setPoints(st * 6, st.toDouble * schrittWeite)
        setPoints((steps + 1) * 6, delta)
        val geom = new MyBufferGeometry()
        val indices = new js.Array[Int]((steps + 1) * 6)
        var current = 0
        for (i <- 0 until (steps + 1) * 2; l <- 0 to 2) {
          indices(current) = if (i % 2 == 0) i + l else i + 2 - l
          current += 1
        }
        geom.setIndex(indices)
        geom.addAttribute("position", new Float32Attribute(apoints, 3))
        geom.computeFaceNormals()
        geom.computeBoundingSphere()
        geom
      } catch {
        case e: Throwable => util.Log.e("Arc Geo:", e);null
      }
    }

  def createTextGeometry(container:ElemContainer,text:String,height:Double,Font:ThreeFont): js.Array[ShapeGeometry] = {
    val shapes=font.generateShapes(text,height*container.scaleRatio/1350d,4)
    shapes.map(el=>new ShapeGeometry(el.asInstanceOf[Shape]))
  }

  def loadTextGeometry(container:ElemContainer, text:String, height:Double, fontName:String, callBack: js.Array[ShapeGeometry] =>Unit):Unit = {
    GraphElem.loadFont(fontName,(font,repaint)=>if(repaint)container.dataUpdated() else internCreateGeometry(container,font))

    def internCreateGeometry(container:ElemContainer,font:ThreeFont):Unit= {
      val shapes=font.generateShapes(text,height*container.scaleRatio/1350d,4)

      val geometries: js.Array[ShapeGeometry] =shapes.map(el=>new ShapeGeometry(el.asInstanceOf[Shape]))
      callBack(geometries)
    }
  }

  def loadFont(fontName:String,callback:(ThreeFont,Boolean)=>Unit):Unit= {
    //helvetiker_bold.typeface.json
    val fname="opensans.json"
    if (font!=null) callback(font,false)
    else if(isFontLoading) {
      fontLoadCallbacks+=callback
      //Log.w("add font callback:"+fontLoadCallbacks.size)
    } else {
      isFontLoading=true
      fontLoader.load(fname, (nfont: ThreeFont) => {
        font=nfont
        println("load font "+fname)
        callback(font,false)
        for(cb<-fontLoadCallbacks) cb(font,false)
        if(fontLoadCallbacks.nonEmpty)
          fontLoadCallbacks.last(null,true)
        fontLoadCallbacks.clear()
        isFontLoading=false
      })
    }
  }

  def getPointsBounds(points:ArrayBuffer[VectorConstant]):BRect =
    if(points==null && points.isEmpty) {Log.e("getPointBounds "+points);null}
    else 	{
      var x1=points.head.x
      var x2=points.head.x
      var y1=points.head.y
      var y2=points.head.y
      if(points.size>1) for(ix <-1 until points.size) {
        val p=points(ix)
        if(p.x<x1) x1=p.x
        if(p.y<y1) y1=p.y
        if(p.x>x2) x2=p.x
        if(p.y>y2) y2=p.y
      }
      BRect(x1,y1,x2,y2)
    }

  val materialCache: mutable.HashMap[Int, MeshBasicMaterial] = collection.mutable.HashMap[Int, MeshBasicMaterial]()

  def getMaterial(color: Int): MeshBasicMaterial = materialCache.getOrElseUpdate(color, {
    //Integer.toHexString(color)
    val c = new Color(((color >> 16) & 0xff).toDouble / 256d, ((color >> 8) & 0xff).toDouble / 256d, (color & 0xff).toDouble / 256d)
    val mat = new MeshBasicMaterial
    mat.color = c
    mat.side = THREE.DoubleSide
    mat
  })

  val selectMaterial: MeshBasicMaterial = getMaterial(255)

  final val lineGeometry = new PlaneBufferGeometry(1, 1)

  val lineCache: mutable.HashMap[(Double, Double), PlaneBufferGeometry] = collection.mutable.HashMap[(Double, Double), PlaneBufferGeometry]()

  def getLine(length: Double, width: Double): PlaneBufferGeometry = lineCache.getOrElseUpdate((length, width), {
    new PlaneBufferGeometry(length, width)
  })


  def calcBounds(list:IterableOnce[GraphElem],controller:ElemContainer):BRect= {
    var x1:Double=Short.MaxValue
    var y1:Double=Short.MaxValue
    var x2:Double=Short.MinValue
    var y2:Double=Short.MinValue
    for(el<-list.iterator) el match {
      case _:GraphElemStub=>
      case g:GraphElem=>
        val l=g.getBounds(controller)
        if(l.minX!=0 || l.minY!=0 || l.maxX!=0 || l.maxY !=0 ) {
          if (l.minX < x1) x1 = l.minX
          if (l.minY < y1) y1 = l.minY
          if (l.maxX > x2) x2 = l.maxX
          if (l.maxY > y2) y2 = l.maxY
        }
    }
    BRect(x1,y1,x2,y2)
  }

  def calcScreenBoundsfrom2Points(p1:VectorConstant, p2:VectorConstant, camera:Camera, res:BoundsContainer): Unit = {
      projectVector.x=p1.x
      projectVector.y=p1.y
      projectVector.z=0
      projectVector.project(camera)
      val sx1=projectVector.x
      val sy1=projectVector.y
      projectVector.x=p2.x
      projectVector.y=p2.y
      projectVector.z=0
      projectVector.project(camera)
      val sx2=projectVector.x
      val sy2=projectVector.y
      res.minX=scala.math.min(sx1,sx2)
      res.maxX=scala.math.max(sx1,sx2)
      res.minY=scala.math.min(sy1,sy2)
      res.maxY=scala.math.max(sy1,sy2)
  }


  def calcScreenBoundsfromPointList(points:Iterator[VectorConstant], camera:Camera, res:BoundsContainer): Unit = {
    res.minX=Double.MaxValue
    res.maxX=Double.MinValue
    res.minY=Double.MaxValue
    res.maxY=Double.MinValue
    for(p<-points) {
      projectVector.x=p.x
      projectVector.y=p.y
      projectVector.z=0
      projectVector.project(camera)
      res.minX=scala.math.min(projectVector.x,res.minX)
      res.maxX=scala.math.max(projectVector.x,res.maxX)
      res.minY=scala.math.min(projectVector.y,res.minY)
      res.maxY=scala.math.max(projectVector.y,res.maxY)
    }
  }


  private[viewer2d] val projectVector=new Vector3()

  /** checks if the given point p checks with the click point at coordinate px,py
   *
   * @param px click pos x
   * @param py click pos y
   * @param dist maximal distance
   * @param p element point
   * @return  a List of one Tuple (Byte =GraphElemConst.HITBOTH,.HITX or .HITY,p)
   */
  def checkHit(px:Double,py:Double,dist:Double,p:VectorConstant):Seq[(Byte,VectorConstant)]={
    val xHit=scala.math.abs(px-p.x)<dist
    val yHit=scala.math.abs(py-p.y)<dist
    if(xHit&&yHit) List((HITBOTH,p))
    else if(xHit)List((HITX,p))
    else if(yHit)List((HITY,p))
    else Nil
  }



}